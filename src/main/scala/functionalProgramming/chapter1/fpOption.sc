sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(x) => x
    }

  def flatMapPM[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(x) => f(x)
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None


  def orElsePM[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(_) => this
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filterPM(f: A => Boolean): Option[A] =
    this match {
      case Some(x) if f(x) => this
      case _ => None
    }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(x => b map(y => f(x,y)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil) : Option[List[A]])((x,y) => map2(x,y)(_ :: _))
//  a match {
//    case Nil => Some(Nil)
//    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
//  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil) : Option[List[B]])((x,y) => map2(f(x),y)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

}

val a = Some(1)

val b = None

a.get
a.filter(x => x == 0)
a.filter(x => x == 1)
b.filter(x => x == 1)
a.getOrElse(2)
b.getOrElse(3)


b.orElse(a)

val listOption : List[Option[Int]] = List(Some(1), Some(2), Some(3))
Option.sequence(listOption)

val list = List(1,2,3,0)

def divide(x : Int) : Option[Double] =
  x match {
    case 0 => None
    case _ => Some(1/x)
  }

Option.traverse(list)(x => divide(x))

Option.sequenceViaTraverse(listOption)



