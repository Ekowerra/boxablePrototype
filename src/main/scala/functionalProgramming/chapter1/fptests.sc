def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, prev: Int, cur: Int): Int = {
    if (n == 0) prev
    else go(n - 1, cur, cur + prev)
  }

  go(n, 0, 1)
}

fib(10)


def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int): Boolean = {
    if (n >= as.length) true
    else {
      if (ordered(as(n - 1), as(n))) go(n + 1)
      else false
    }
  }

  go(1)
}

val list = Array(1, 2, 3, 6)
def f(i: Int, j: Int): Boolean = i < j
isSorted(list, f)

val listChar = Array('a', 'b', 'c', 'D')
'a'.toInt
'd'.toInt
'D'.toInt
def g(c1: Char, c2: Char): Boolean = c1.toInt < c2.toInt
isSorted(listChar, g)


def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  a => f(a, _)


def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

val c = uncurry(curry(f))
val c2 = f _

c.equals(c2)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](a: A, as: List[A]): List[A] = {
    as match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) {
      l
    } else {
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) l else dropWhile(xs, f)
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, xs) => Cons(h, init(xs))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumLeft(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def lengthLeft[A](as : List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as : List[A]) : List[A] = {
    foldLeft(as, List[A]())((acc, e) => Cons(e,acc))
  }

  def foldLeftByRight[A,B](as : List[A], z : B)(f: (B,A) => B): B= {
    foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  }

  def foldRightByLeft[A,B](as: List[A], z:B)(f:(A,B) => B): B = {
    foldLeft(as, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  }

  def append[A](as : List[A], ls : List[A]): List[A] =
    foldRightByLeft(as, ls)(Cons(_,_))

  def concatenate[A](ass: List[List[A]]) : List[A] = {
    foldLeft(ass, Nil:List[A])(append)
  }

  def addOne(as : List[Int]) : List[Int] = {
    foldRight(as, Nil:List[Int])((e, acc) => Cons(e + 1, acc))
  }

  def doubleToString(as : List[Double]) : List[String] =
    foldRight(as, Nil:List[String])((e,acc) => Cons(e.toString, acc))

  def map[A,B](as : List[A])( f : A => B): List[B] =
    foldRightByLeft(as, Nil:List[B])((e, acc) => Cons(f(e), acc))

  def filter[A](as : List[A])(f: A => Boolean): List[A] =
    foldRightByLeft(as, Nil:List[A])((e, acc) => if (f(e)) Cons(e, acc) else acc)

  def flatMap[A, B](as : List[A])(f : A => List[B]): List[B] =
    foldRightByLeft(as, Nil:List[B])((e,acc) => append(f(e), acc))
  // concat(map(l)(f))

  def filterWithFlatMap[A](as : List[A])(f: A => Boolean): List[A]=
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addInts(as : List[Int], ls: List[Int]): List[Int] =
    (as, ls) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(x, xs), Cons(y,ys)) => Cons(x+y,addInts(xs,ys))
    }

  def zipWith[A,B,C](as : List[A], bs : List[B])(f: (A,B) => C): List[C] =
    (as,bs) match {
      case (Nil, _) | (_, Nil) => Nil:List[C]
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y),zipWith(xs,ys)(f))
    }

  def hasSubsequence[A](sup: List[A], sub:List[A]): Boolean =
    (sup, sub) match {
      case (Cons(x,xs), Cons(y, ys)) => if (x==y) hasSubsequence(xs,ys) else hasSubsequence(xs, sub)
      case (_, Nil) => true
      case (Nil, _) => false
    }
}

val x5 = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

List.tail(List(1, 2, 3, 4, 5))

List.setHead(10, List(1, 2, 3, 4, 5))

List.drop(List(1, 2, 3, 4, 5), 3)

List.dropWhile(List(1, 2, 3, 4, 5), curry(f)(3))

List.init(List(1, 2, 3, 4, 5))

List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

List.length(List(1, 2, 3, 4, 5, 7))
List.lengthLeft(List(1, 2, 3, 4, 5, 7))

List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _)
List.sumLeft(List(1, 2, 3, 4, 5))
List.foldLeft(List(1, 2, 3, 4, 5), 1)(_ * _)
List.productLeft(List(1, 2, 3, 4, 5))
List.foldLeft(List("H", "e", "l", "l", "o", " ", "W", "o", "r", "l", "d", "!"), "")(_ + _)


List.reverse(List(1,2,3))

List.foldRightByLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

List.append(List(1,2,3), List(4,5,6))

List.concatenate(List(List(1,2), List(3,4,5), List(7,8,9)))

List.addOne(List(1,2,3,4,5,6,7,8,9))

List.doubleToString(List(1.0,2.0,3.0,4.0))

List.map(List(1,2,3,4,5))( (x: Int) => (x + 2).toString)

List.filter(List(1,2,3,4,5))((x: Int) => x!=4)

List.flatMap(List(1,2,3))(i => List(i,i))

List.addInts(List(1,2,3), List(4,5,6))

List.zipWith(List(1,2,3), List(4,5,6))(_ + _)

List.hasSubsequence(List(1,2,3,4,5), List(1p)m)