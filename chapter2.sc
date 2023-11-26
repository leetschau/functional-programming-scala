// exercise 2.1

def fib(n: Int): Int = n match
  case 1 => 0
  case 2 => 1
  case x => fib(n - 2) + fib(n - 1)


val n = 15
println(s"fib($n) = ${fib(n)}")
