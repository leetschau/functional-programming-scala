// exercise 2.2

val <T> List<T>.tail: List<T>
  get() = drop(1)

val <T> List<T>.head: T
  get() = first()

fun <A> isSorted(data: List<A>, order: (A, A) -> Boolean): Boolean = when (data) {
  listOf(3) + tail -> true
  else -> false
}

println(isSorted(listOf(3,2,1), {x, y -> x > y}))
