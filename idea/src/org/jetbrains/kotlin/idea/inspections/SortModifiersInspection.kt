/*
 * Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.inspections

import com.intellij.codeInspection.*
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElementVisitor
import org.jetbrains.kotlin.lexer.KtModifierKeywordToken
import org.jetbrains.kotlin.psi.KtModifierList
import org.jetbrains.kotlin.psi.KtModifierListOwner
import org.jetbrains.kotlin.psi.addRemoveModifier.sortModifiers
import org.jetbrains.kotlin.psi.modifierListVisitor
import org.jetbrains.kotlin.psi.psiUtil.allChildren

class SortModifiersInspection : AbstractKotlinInspection(), CleanupLocalInspectionTool {

    override fun buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean, session: LocalInspectionToolSession): PsiElementVisitor {
        return modifierListVisitor(fun(list) {
            val modifiers = list.allChildren.toList().mapNotNull { it.node.elementType as? KtModifierKeywordToken }.toList()
            if (modifiers.isEmpty()) return

            val sortedModifiers = sortModifiers(modifiers)
            if (modifiers == sortedModifiers) return

            holder.registerProblem(list,
                                   "Non-canonical modifiers order",
                                   ProblemHighlightType.WEAK_WARNING,
                                   SortModifiersFix(sortedModifiers)
            )
        })
    }
}

private class SortModifiersFix(private val modifiers: List<KtModifierKeywordToken>) : LocalQuickFix {
    override fun getName() = "Sort modifiers"

    override fun getFamilyName() = name

    override fun applyFix(project: Project, descriptor: ProblemDescriptor) {
        val list = descriptor.psiElement as? KtModifierList ?: return
        val owner = list.parent as? KtModifierListOwner ?: return

        modifiers.forEach { owner.removeModifier(it) }
        modifiers.forEach { owner.addModifier(it) }
    }
}

