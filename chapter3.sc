enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])


object MyList:
  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(ints: MyList[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: MyList[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def tail[A](alist: MyList[A]): MyList[A] = alist match
    case Nil => Nil
    case Cons(x, xs) => xs

  def setHead[A](alist: MyList[A], head: A): MyList[A] = (alist, head) match
    case (Nil, _) => Nil
    case (Cons(x, xs), head) => Cons(head, xs)

  def drop[A](as: MyList[A], n: Int): MyList[A] = (as, n) match
    case (Nil, _) => Nil
    case (as, 0) => as
    case (Cons(x, xs), n) => drop(xs, n - 1)

  def dropWhile[A](as: MyList[A], f: A => Boolean): MyList[A] = (as, f) match
    case (Nil, _) => Nil
    case (Cons(x, xs), f) => if (f(x)) dropWhile(xs, f) else Cons(x, xs)

  def init[A](as: MyList[A]): MyList[A] = as match
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))


// exercise 3.1
import MyList.*

val result = MyList(1,2,3,4,5) match
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)

assert(result == 3)

// exercise 3.2
val myl = MyList(1, 2, 3, 4)
assert(tail(myl) == MyList(2, 3, 4))

// exercise 3.3
assert(setHead(myl, 8) == MyList(8, 2, 3, 4))

// exercise 3.4
assert(drop(myl, 3) == MyList(4))

// exercise 3.5
assert(dropWhile(myl, _ < 3) == MyList(3, 4))

// exercise 3.6
assert(init(myl) == MyList(1, 2, 3))
