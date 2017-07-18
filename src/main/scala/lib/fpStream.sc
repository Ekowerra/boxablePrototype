def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if (cond) onTrue else onFalse

if2(false, println(1), println(2))

def maybeTwice(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j + j else 0
}

maybeTwice(true, {
  println("hi");
  1 + 41
})

sealed trait Stream[+A] {

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

  def toListRecursive: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toListRecursive
    }

  def toList: List[A] = {
    @annotation.tailrec
    def go(list: List[A], stream: Stream[A]): List[A] =
      stream match {
        case Empty => list.reverse
        case Cons(h, t) => go(h() :: list, t())
      }

    go(Nil, this)
  }

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => Cons(h, () => t().take(n - 1))
      case Empty => Empty
      case Cons(h, _) if n == 1 => Cons(h, () => Empty)
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) if n > 1 => t().drop(n - 1)
      case Cons(_, t) if n == 1 => t()
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsViaFR(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def takeWhileViaFR(p : A => Boolean): Stream[A] =
    foldRight(Empty : Stream[A])((h,t) => if(p(h)) Cons(() => h,() => t) else t)

  def headOptionViaFR: Option[A] =
    foldRight(None : Option[A])((a,_) => Some(a))

  def map[B](f :A => B): Stream[B] =
    foldRight(Empty:Stream[B])((h,t) => Cons(() => f(h),() => t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((h,t) => if(p(h)) Cons(() => h, () =>  t) else t)


//J'ignore pourquoi ici append est déclaré comme non-strict ->
// surement du au fait que ls est un Stream (lazy list => on evalue au
// besoin et donc pas à l'entrée dans la fonction)
  def append[B >: A](ls: => Stream[B]): Stream[B] =
    foldRight(ls)((h,t) => Cons(() => h, () => t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty : Stream[B])((h,t) => f(h) append t)

  def mapViaUF[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUF(n : Int): Stream[A] =
    Stream.unfold((this,n)) {
      case (Cons(h,t), x1) if x1 > 0 => Some(h(),(t(),x1-1))
      case _ => None
    }

  def takeWhileViaUF(p : A => Boolean): Stream[A] =
    Stream.unfold(this){
      case Cons(h,t) if p(h()) => Some(h(),t())
      case _ => None
    }

  def zipWith[B,C](as : => Stream[B])(f : (A,B) => C) : Stream[C] =
    Stream.unfold((this,as)){
      case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
      case _ => None
    }

  def zipAll[B](s2 : Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this,s2)){
      case (Cons(h1,t1),Cons(h2,t2)) => Some((Some(h1()), Some(h2())),(t1(),t2()))
      case (Cons(h1,t1), Empty) => Some((Some(h1()), None),(t1(),Empty))
      case (Empty, Cons(h2,t2)) => Some((None, Some(h2())),(Empty,t2()))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
//    !Stream.unfold((this,s)){
//      case (Cons(h1,t1), Cons(h2,t2)) if h1() == h2() => Some(false, (t1(),t2()))
//      case (Cons(h1,t1), Cons(h2,t2)) if h1() != h2() => Some(true, (t1(),t2()))
//      case _ => None
//    }.exists(x => x)

    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this)({
      case Cons(h,t) => Some(Cons(h,t), t())
      case _ => None
    }) append Stream(Stream.empty)

  def hasSubsequence[A](s:Stream[A]):Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](s : B)(f : (A, => B) => B) : Stream[B] =
    tails.foldRight(Empty: Stream[B])((e,acc) =>
      Cons(() =>e.foldRight(s)((x,y) => f(x,y)),() => acc))
//  foldRight((s, Stream(s)))((a, p0) => {
//    // p0 is passed by-name and used in by-name args in f and cons.
  // So use lazy val to ensure only one evaluation...
//    lazy val p1 = p0
//    val b2 = f(a, p1._1)
//    (b2, Stream.cons(b2, p1._2))
//  })._2
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a : A): Stream[A] =
    cons(a,constant(a))

  def from(n : Int): Stream[Int] =
    cons(n, from(n+1))

  val fibs: Stream[Int] = {
    def go(n1 : Int, n2: Int): Stream[Int] =
      cons(n1,go(n2, n1+n2))

    go(0,1)
  }

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    // utiliser une boucle tail-recursive provoque une compilation sans fin
    // probablement du au fait que jamais on obtient la réponse

//    def go(h : Option[(A,S)], t : Stream[A]) : Stream[A] =
//      h match {
//        case None => t
//        case Some(tuple) => go(f(tuple._2), cons(tuple._1,t))
//      }
//    go(f(z),Empty)

    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  val fibsViaUF: Stream[Int] =
    unfold((0,1))(n12 => Some((n12._1, (n12._2, n12._1 + n12._2))))

  def fromViaUF(n : Int): Stream[Int] =
    unfold(n)(x => Some((x, x+1)))

  def constantViaUF[A](a : A) : Stream[A] =
    unfold(a)(_ => Some((a,a)))

  val ones = constantViaUF(1)

}

val x = Cons(() => {
  println("world");
  1
}, () => Empty)
val h1 = x.headOption
val h2 = x.headOption

val y = Stream({
  println("world");
  1
})
val hy1 = y.headOption
val hy2 = y.headOption

val stream = Stream({println("hi1");1},
  {println("hi2");2}, {println("hi3");3}, {println("hi4");4},
  {println("hi5");5})
stream.toList
stream.take(3).toList
stream.drop(3).toList
stream.takeWhile(x => x <= 3).toList
stream.takeWhileViaFR(x => x <= 3).toList
stream.takeWhileViaUF(_ <= 3).toList
stream.forAll(x => x < 4)

val constream = Cons(() => {println("hi1");1},
  () => Cons(() => {println("hi2");2},
    () => Cons(() => {println("hi3");3},
      () => Cons(() => {println("hi4");4},
        () => Cons(() => {println("hi5");5},
          () => Empty)))))

constream.forAll(x => x < 4)
constream.forAll(x => x < 3)

stream.headOptionViaFR
val emptyStream : Stream[Int] = Empty
emptyStream.headOptionViaFR
stream.append(stream.takeWhile(x => x < 4)).toList


Stream(1,2,3,4).map( _ + 10).filter(_ % 2 == 0).map( _ * 3).toList
Stream(1,2,3,4).mapViaUF( _ + 10).filter(_ % 2 == 0).map( _ * 3).toList

val ones: Stream[Int] = Stream.cons(1, ones)

ones.take(5).toList
ones.exists(_ % 2 != 0)
//ones.exists(_ % 2 == 0)

Stream.constant(2).take(5).toList
Stream.from(1).filter(_ % 2 == 0).takeWhile(_ < 10).toList
Stream.fromViaUF(1).filter(_ % 2 == 0).takeWhile(_ < 10).toList

Stream.fibs.take(7).toList
Stream.fibsViaUF.take(7).toList

stream append Stream.ones take 15 toList

stream.takeViaUF(3).toList

stream.zipWith(stream)(_ + _).toList

stream.zipAll(ones).take(7).toList

Stream(1,2,3,4,5).startsWith(Stream(1,2,3,4,5,6,7))
Stream(1,2,3,4,5).tails.map(_.toList).toList
Stream(1,2,3,4,5).hasSubsequence(Stream(3,4,5))

Stream(1,2,3).scanRight(0)(_+_).take(4).toList

