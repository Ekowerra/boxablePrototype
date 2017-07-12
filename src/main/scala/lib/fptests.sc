def fib(n : Int): Int = {
  @annotation.tailrec
  def go(n : Int, prev : Int, cur : Int): Int = {
    if (n==0) prev
    else go(n-1, cur, cur+prev)
  }
  go(n, 0, 1)
}

fib(10)


def isSorted[A](as : Array[A], ordered : (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n : Int): Boolean = {
    if (n >= as.length) true
    else {
      if (ordered(as(n-1), as(n))) go(n+1)
      else false
    }
  }
  go(1)
}

val list = Array(1,2,3,6)
def f(i : Int, j : Int): Boolean = i > j
isSorted(list, f)

val listChar = Array('a','b','c','D')
'a'.toInt
'd'.toInt
'D'.toInt
def g(c1: Char, c2: Char): Boolean = c1.toInt < c2.toInt
isSorted(listChar, g)


def curry[A,B,C](f: (A,B) => C): A => (B => C) =
  a => f(a, _)


def uncurry[A,B,C](f: A => B => C): (A,B) => C =
  (a,b) => f(a)(b)

val c = uncurry(curry(f))
val c2 = f _

c.equals(c2)

def compose[A,B,C](f : B => C, g : A => B): A => C =
  a => f(g(a))


