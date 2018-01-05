/*
 * Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.inspections

import com.intellij.codeInspection.*
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiElementVisitor
import com.intellij.psi.SmartPsiElementPointer
import com.intellij.refactoring.rename.inplace.VariableInplaceRenameHandler
import org.jetbrains.kotlin.idea.caches.resolve.analyze
import org.jetbrains.kotlin.idea.core.ShortenReferences
import org.jetbrains.kotlin.idea.util.getReceiverTargetDescriptor
import org.jetbrains.kotlin.idea.util.getResolutionScope
import org.jetbrains.kotlin.incremental.components.NoLookupLocation
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.createSmartPointer
import org.jetbrains.kotlin.psi.psiUtil.getOrCreateParameterList
import org.jetbrains.kotlin.psi.psiUtil.startOffset
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.BindingContext.FUNCTION
import org.jetbrains.kotlin.resolve.calls.callUtil.getResolvedCall
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameSafe
import org.jetbrains.kotlin.resolve.lazy.BodyResolveMode
import org.jetbrains.kotlin.resolve.scopes.LexicalScope
import org.jetbrains.kotlin.resolve.scopes.utils.findVariable

class ScopeFunctionConversionInspection : AbstractKotlinInspection() {
    override fun buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean, session: LocalInspectionToolSession): PsiElementVisitor {
        return object : KtVisitorVoid() {
            override fun visitCallExpression(expression: KtCallExpression) {
                super.visitCallExpression(expression)
                val callee = expression.calleeExpression as? KtNameReferenceExpression ?: return
                if (callee.getReferencedName() == "apply" && expression.lambdaArguments.isNotEmpty()) {
                    val bindingContext = callee.analyze(BodyResolveMode.PARTIAL)
                    val resolvedCall = callee.getResolvedCall(bindingContext) ?: return
                    val fqName = resolvedCall.resultingDescriptor.fqNameSafe
                    if (!fqName.isRoot && fqName.parent().asString() == "kotlin") {
                        holder.registerProblem(
                            callee,
                            "Call can be replaced with another scope function",
                            ProblemHighlightType.INFORMATION,
                            ConvertToAlsoFix()
                        )
                    }
                }
            }
        }
    }
}

class ConvertToAlsoFix : LocalQuickFix {
    override fun getFamilyName() = "Convert to 'also'"

    override fun applyFix(project: Project, problemDescriptor: ProblemDescriptor) {
        val callee = problemDescriptor.psiElement as KtNameReferenceExpression
        val callExpression = callee.parent as? KtCallExpression ?: return
        val bindingContext = callExpression.analyze()

        val lambda = callExpression.lambdaArguments.firstOrNull() ?: return
        val parameterToRename = replaceThisWithIt(bindingContext, lambda)
        callee.replace(KtPsiFactory(project).createExpression("also") as KtNameReferenceExpression)
        removeThisLabels(lambda)

        if (parameterToRename != null && !ApplicationManager.getApplication().isUnitTestMode) {
            parameterToRename.startInPlaceRename()
        }
    }
}

private fun replaceThisWithIt(bindingContext: BindingContext, lambdaArgument: KtLambdaArgument): KtParameter? {
    val project = lambdaArgument.project
    val factory = KtPsiFactory(project)
    val functionLiteral = lambdaArgument.getLambdaExpression().functionLiteral
    val functionDescriptor = bindingContext[FUNCTION, functionLiteral] ?: return null
    val lambdaExtensionReceiver = functionDescriptor.extensionReceiverParameter
    val lambdaDispatchReceiver = functionDescriptor.dispatchReceiverParameter

    var parameterName = "it"
    var parameterToRename: KtParameter? = null
    val scopes = mutableSetOf<LexicalScope>()
    if (needUniqueNameForParameter(lambdaArgument, scopes)) {
        parameterName = findUniqueParameterName(scopes)
    }

    val callsToReplace = mutableListOf<SmartPsiElementPointer<KtCallExpression>>()
    val nameReferencesToReplace = mutableListOf<SmartPsiElementPointer<KtSimpleNameExpression>>()
    val thisReferencesToReplace = mutableListOf<SmartPsiElementPointer<KtThisExpression>>()

    lambdaArgument.accept(object : KtTreeVisitorVoid() {
        override fun visitSimpleNameExpression(expression: KtSimpleNameExpression) {
            super.visitSimpleNameExpression(expression)
            val resolvedCall = expression.getResolvedCall(bindingContext) ?: return
            val dispatchReceiverTarget = resolvedCall.dispatchReceiver?.getReceiverTargetDescriptor(bindingContext)
            val extensionReceiverTarget = resolvedCall.extensionReceiver?.getReceiverTargetDescriptor(bindingContext)
            if (dispatchReceiverTarget == functionDescriptor || extensionReceiverTarget == functionDescriptor) {
                val parent = expression.parent
                if (parent is KtCallExpression && expression == parent.calleeExpression) {
                    callsToReplace.add(parent.createSmartPointer())
                } else if (parent is KtQualifiedExpression && parent.receiverExpression is KtThisExpression) {
                    // do nothing
                } else {
                    nameReferencesToReplace.add(expression.createSmartPointer())
                }
            }
        }

        override fun visitThisExpression(expression: KtThisExpression) {
            val resolvedCall = expression.getResolvedCall(bindingContext) ?: return
            if (resolvedCall.resultingDescriptor == lambdaDispatchReceiver ||
                resolvedCall.resultingDescriptor == lambdaExtensionReceiver) {
                thisReferencesToReplace.add(expression.createSmartPointer())
            }
        }
    })

    if (!callsToReplace.isEmpty() || !nameReferencesToReplace.isEmpty() || !thisReferencesToReplace.isEmpty()) {
        if (parameterName != "it") {
            val lambdaParameterList = functionLiteral.getOrCreateParameterList()
            val parameterToAdd = factory.createLambdaParameterList(parameterName).parameters.first()
            parameterToRename = lambdaParameterList.addParameterBefore(parameterToAdd, lambdaParameterList.parameters.firstOrNull())
        }

        for (namePointer in nameReferencesToReplace) {
            namePointer.element?.let { element ->
                element.replace(factory.createExpression("$parameterName.${element.getReferencedName()}"))
            }
        }
        for (thisPointer in thisReferencesToReplace) {
            thisPointer.element?.replace(factory.createExpression(parameterName))
        }
        for (callPointer in callsToReplace) {
            callPointer.element?.let { element ->
                element.replace(factory.createExpressionByPattern("$0.$1", parameterName, element))
            }
        }
    }

    return parameterToRename
}

private fun needUniqueNameForParameter(
    lambdaArgument: KtLambdaArgument,
    scopes: MutableSet<LexicalScope>
): Boolean {
    val resolutionScope = lambdaArgument.getResolutionScope()
    scopes.add(resolutionScope)
    var needUniqueName = false
    if (resolutionScope.findVariable(Name.identifier("it"), NoLookupLocation.FROM_IDE) != null) {
        needUniqueName = true
        // Don't return here - we still need to gather the list of nested scopes
    }

    lambdaArgument.accept(object : KtTreeVisitorVoid() {
        override fun visitDeclaration(dcl: KtDeclaration) {
            super.visitDeclaration(dcl)
            checkNeedUniqueName(dcl)
        }

        override fun visitLambdaExpression(lambdaExpression: KtLambdaExpression) {
            super.visitLambdaExpression(lambdaExpression)
            lambdaExpression.bodyExpression?.statements?.firstOrNull()?.let { checkNeedUniqueName(it) }
        }

        private fun checkNeedUniqueName(dcl: KtElement) {
            val nestedResolutionScope = dcl.getResolutionScope()
            scopes.add(nestedResolutionScope)
            if (nestedResolutionScope.findVariable(Name.identifier("it"), NoLookupLocation.FROM_IDE) != null) {
                needUniqueName = true
            }
        }
    })

    return needUniqueName
}

private fun KtElement.startInPlaceRename() {
    val project = project
    val document = containingFile.viewProvider.document ?: return
    val editor = FileEditorManager.getInstance(project).selectedTextEditor ?: return
    if (editor.document == document) {
        PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(editor.document)

        editor.caretModel.moveToOffset(startOffset)
        VariableInplaceRenameHandler().doRename(this, editor, null)
    }
}

private fun removeThisLabels(lambdaArgument: KtLambdaArgument) {
    ShortenReferences { ShortenReferences.Options(removeThisLabels = true) }.process(lambdaArgument) { element ->
        if (element is KtThisExpression && element.getLabelName() != null)
            ShortenReferences.FilterResult.PROCESS
        else
            ShortenReferences.FilterResult.GO_INSIDE
    }
}

private fun findUniqueParameterName(resolutionScopes: Collection<LexicalScope>): String {
    var parameterName = "p"
    if (resolutionScopes.any { it.findVariable(Name.identifier(parameterName), NoLookupLocation.FROM_IDE) != null }) {
        for (index in generateSequence(0) { it + 1 }) {
            parameterName = "p$index"
            if (resolutionScopes.any { it.findVariable(Name.identifier(parameterName), NoLookupLocation.FROM_IDE) == null }) {
                break
            }
        }
    }
    return parameterName
}

