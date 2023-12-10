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

  // my solution:
  def setHead[A](alist: MyList[A], head: A): MyList[A] = (alist, head) match
    case (Nil, _) => Nil
    case (Cons(x, xs), head) => Cons(head, xs)

  def setHead2[A](alist: MyList[A], head: A): MyList[A] = alist match
    case Nil => sys.error("setHead on empty list")
    case Cons(x, xs) => Cons(head, xs)

  def drop[A](as: MyList[A], n: Int): MyList[A] = (as, n) match
    case (Nil, _) => Nil
    case (as, 0) => as
    case (Cons(x, xs), n) => drop(xs, n - 1)

  // my solution:
  def dropWhile[A](as: MyList[A], f: A => Boolean): MyList[A] = (as, f) match
    case (Nil, _) => Nil
    case (Cons(x, xs), f) => if (f(x)) dropWhile(xs, f) else Cons(x, xs)

  def dropWhile2[A](as: MyList[A], f: A => Boolean): MyList[A] = as match
    case Cons(x, xs) if f(x) => dropWhile2(xs, f)
    case _ => as

  def init[A](as: MyList[A]): MyList[A] = as match
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))

  def foldRight[A, B](as: MyList[A], acc: B, f: (A, B) => B): B = as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def contains[A](as: MyList[A], e: A): Boolean = (as, e) match
    case (Nil, _) => false
    case (Cons(x, xs), e) =>
      if (x == e) true
      else contains(xs, e)

  def length[A](as: MyList[A]): Int = as match
    case Nil => 0
    case Cons(x, xs) => 1 + length(xs)

  def foldLeft[A, B](as: MyList[A], acc: B, f: (B, A) => B): B = as match
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sum2(ints: MyList[Int]): Int = foldLeft(ints, 0, _ + _)

  def product2(doubles: MyList[Double]): Double = foldLeft(doubles, 1.0, _ * _)

  def length2[A](as: MyList[A]): Int = foldLeft(as, 0, (acc, x) => acc + 1)

  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, Nil: MyList[A], (acc, x) => Cons(x, acc))
  // def reverse2[Int](as: MyList[Int]): MyList[Int] = foldLeft(as, MyList(0): MyList[Int], (acc, x) => Cons(x, acc))
  // why above definition not work? why Nil is the only valid initial value of `acc`?
  // Maybe it's because the only terminate condition for foldLeft is `Nil => acc`?

  def foldRight2[A, B](as: MyList[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (a, b) => f(b, a))

  // my solution:
  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    reverse(foldRight2(reverse(a2), reverse(a1), Cons(_, _)))

  def append2[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldRight2(a1, a2, Cons(_, _))

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

// exercise 3.7

// No, it won't stop when encounter 0.0 in `foldRight(..., product)`.
// the implementation with shortcut:
def productViaFoldRight(ns: MyList[Double]): Double =
  if contains(ns, 0.0) then 0.0
  else foldRight(ns, 1.0, _ * _)

assert(foldRight(MyList(1, 0, 3), 1.0, _ * _) == 0.0)

// exercise 3.8
// foldRight(xs, Nil, Cons(_, _)) equals to the data constructor of MyList
assert(foldRight(myl, Nil: MyList[Int], Cons(_, _)) == myl)

// exercise 3.9
assert(length(myl) == 4)

// exercise 3.10
assert(foldLeft(myl, 0, _ + _) == 10)

// exercise 3.11
assert(sum2(myl) == 10)
val myfl = MyList(1.0, 2.0, 3.0, 4.0)
assert(product2(myfl) == 24.0)
assert(length2(myl) == 4)

// exercise 3.12
assert(reverse(myl) == MyList(4, 3, 2, 1))

// exercise 3.13
assert(foldRight2(myl, 2, _ + _) == 12)

// exercise 3.14
assert(append(myl, MyList(8, 9)) == MyList(1, 2, 3, 4, 8, 9))
assert(append2(myl, MyList(8, 9)) == MyList(1, 2, 3, 4, 8, 9))

// exercise 3.15

