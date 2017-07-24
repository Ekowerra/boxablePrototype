package lib.chapter3.monoid

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z:B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z:B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_::_)
}

object ListF extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
    Monoid.foldMap(as,mb)(f)
  }
}

object IndexedSeqF extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = {
    Monoid.foldMapV(as,mb)(f)
  }
}

object StreamF extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = {
    concatenate(as map f)(mb)
  }
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object TreeF extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  }

  override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = {
    as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  }
}

object OptionF extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = {
    as match {
      case Some(a) => f(a)
      case None => mb.zero
    }
  }
}
