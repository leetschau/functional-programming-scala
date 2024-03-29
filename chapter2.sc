// exercise 2.1

def fib(n: Int): Int = n match
  case 1 => 0
  case 2 => 1
  case x => fib(n - 2) + fib(n - 1)


val n = 15
println(s"fib($n) = ${fib(n)}")

// exercise 2.2

def isSorted[A](data: List[A], order: (A, A) => Boolean): Boolean =
  data match
    case h1 :: h2 :: Nil => order(h1, h2)
    case h1 :: h2 :: tail => order(h1, h2) && isSorted(h2 :: tail, order)
    case _ => true  // lists whose length < 2 are always sorted

println(s"isSorted(1,2,3) by greater than: ${isSorted(List(1, 2, 3), _ > _)}")
println(s"isSorted(1,2,1) by greater than: ${isSorted(List(1, 2, 1), _ > _)}")
println(s"isSorted(3,2,1) by less than: ${isSorted(List(3, 2, 1), _ < _)}")
println(s"isSorted(1,2,3) by less than: ${isSorted(List(1, 2, 3), _ < _)}")

val isGreater: (Int, Int) => Boolean = _ > _

println(s"3 is greater than 2? ${isGreater(3, 2)}")

// note the results here are different from the book.
// IMO the results given in the book, for example `isSorted(Array(1, 2, 3), _ > _)`
// returns `true` is wrong: the list 1,2,3 is not sorted with regard to
// "the first element is greater than the 2nd element".

// another solution from https://github.com/ruivalentemaia/fpscala:
def isSorted2[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
	@annotation.tailrec
	def loop(n:Int) : Boolean = 
		if(n >= as.length - 1) true
		else if (ordered(as(n),as(n+1))) loop(n+1)
		else false
	loop(0)
}

// exercise 2.3
def partial1[A, B, C](f: (A, B) => C, b: B): A => C = (a: A) => f(a, b)
val greaterThan3: (x: Int) => Boolean = partial1(isGreater, 3)
println(s"5 > 3? ${greaterThan3(5)}")

def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)
val isGreaterCurried = curry(isGreater)
val lessThan3: (x: Int) => Boolean = isGreaterCurried(3)  // the subject is the 1st paramter of function isGreater, the object is the 2nd parameter, so the comparison direction is reversed.
println(s"2 < 3? ${lessThan3(2)}")

// exercise 2.4
def uncurry[A, B, C](f: A => B => C): (A, B) => C = (x: A, y: B) => f(x)(y)

// exercise 2.5
def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
