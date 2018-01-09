// WITH_RUNTIME
import kotlin.coroutines.experimental.*
import kotlin.coroutines.experimental.intrinsics.*

var p: Int = 5846814
private suspend fun optimized() {
    val c = { c: Continuation<Unit> ->
        if (p > 52158) Unit else COROUTINE_SUSPENDED
    }

    return suspendCoroutineOrReturn(c)
}

private suspend fun nonOptimized() {
    return suspendCoroutineOrReturn { c ->
        if (p > 52158) Unit else COROUTINE_SUSPENDED
    }
}