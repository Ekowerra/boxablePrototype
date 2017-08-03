sealed trait Either[+E,+A] {
  def map[B](f : A => B): Either[E, B] =
    this match {
      case Left(x) => Left(x)
      case Right(x) => Right(f(x))
    }

  def flatMap[EE >: E, B](f : A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(x) => Left(x)
      case Right(x) => f(x)
    }

  def orElse[EE >: E, B >: A](b : => Either[EE,B]): Either[EE,B] =
    this match {
      case Left(_) => b
      case Right(x) => Right(x)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE,C] =
    for {
      x <- this
      y <- b
    } yield f(x,y)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value : A) extends Either[Nothing, A]

object Either {

  def sequence[E,A](es : List[Either[E,A]]): Either[E,List[A]] =
    es.foldRight(Right(Nil) : Either[E,List[A]])((x,y) => x.map2(y)(_ :: _))

  def traverse[E,A,B](as: List[A])(f : A => Either[E,B]): Either[E,List[B]] =
    as.foldRight(Right(Nil):Either[E,List[B]])((x,y) => f(x).map2(y)(_ :: _))

  def sequenceViaTraverse[E,A](es : List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)
}


//une implémentation proposée pour la structure Partial qui collecte les erreurs
sealed trait Partial[+E,+A] {
  def map[B](f : A => B): Partial[E, B] =
    this match {
      case Errors(x) => Errors(x)
      case Success(x) => Success(f(x))
    }

  def flatMap[EE >: E, B](f : A => Partial[EE, B]): Partial[EE, B] =
    this match {
      case Errors(x) => Errors(x)
      case Success(x) => f(x)
    }

  def orElse[EE >: E, B >: A](b : => Partial[EE,B]): Partial[EE,B] =
    this match {
      case Errors(_) => b
      case Success(x) => Success(x)
    }

  def map2[EE >: E, B, C](b: Partial[EE, B])(f: (A,B) => C): Partial[EE,C] =
    (this, b) match {
      case (Errors(x), Errors(y)) => Errors(x ++ y)
      case (Errors(x), _) => Errors(x)
      case (_, Errors(x)) => Errors(x)
      case (Success(x),Success(y)) => Success(f(x,y))
    }
}
case class Errors[+E](get: Seq[E]) extends Partial[E,Nothing]
case class Success[+A](get: A) extends Partial[Nothing,A]