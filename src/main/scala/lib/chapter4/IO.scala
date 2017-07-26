package lib.chapter4

import lib.chapter2.parallelism.NonBlocking.Par
import lib.chapter3.applicative.Monad
import lib.chapter4.Free.~>
import lib.chapter4.IO.unit

/*
Cette implémentation permet l'externalisation des outputs mais pas des inputs.

trait IO {self =>
  def run: Unit
def ++(io: IO):IO = new IO {
  override def run: Unit = {self.run;io.run}
}}

object IO {
  def empty: IO = new IO {
    override def run: Unit = ()
  }
}*/


sealed trait IO[A] {
  self =>
  def run: A

  def map[B](f: A => B): IO[B] = flatMap( f andThen (Return_(_)))


  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap_(this, f)
}
case class Return_[A](a: A) extends IO[A] {
  override def run: A = ???
}
case class Suspend_[A](resume: () => A) extends IO[A] {
  override def run: A = ???
}
case class FlatMap_[A,B](sub: IO[A], k:A => IO[B]) extends IO[B] {
  override def run: B = ???
}

object IO extends Monad[IO]{
  override def unit[A](a: => A): IO[A] = new IO[A] {
    override def run: A = a
  }

  override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = ma flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

}


sealed trait TailRec[A] {
  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap_2(this, f)

  def map[B](f: A => B): TailRec[B] =
    flatMap(f andThen (Return_2(_)))
}

case class Return_2[A](a: A) extends TailRec[A]
case class Suspend_2[A](resume: () => A) extends TailRec[A]
case class FlatMap_2[A,B](sub: TailRec[A], k:A => TailRec[B]) extends TailRec[B]

sealed trait Free[F[_], A] {
  def map[B](f : A => B) : Free[F,B] =
    flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => Free[F,B]): Free[F,B] =
    FlatMap(this, f)
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_], A, B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Free {

  type Tailrec[A] = Free[Function0,A]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      override def unit[A](a: => A): Free[F, A] = new Return[F, A](a)
      override def flatMap[A,B](fa: Free[F,A])(f: A => Free[F,B]): Free[F,B] = fa flatMap f

      def apply[A](a: => A): Free[F,A] = unit(a)
    }

  @annotation.tailrec
  def runTrampoline[A](free: Free[Function0,A]): A =
    free match {
      case Return(a) => a
      case Suspend(s) => s()
      case FlatMap(x, f) => x match {
        case Return(a) => runTrampoline(f(a))
        case Suspend(r) => runTrampoline(f(r()))
        case FlatMap(y,g) => runTrampoline(y flatMap (a => g(a) flatMap f))
      }
    }

  @annotation.tailrec
  def step[F[_], A](free: Free[F,A]): Free[F,A] = free match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap(a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def run[F[_], A](free: Free[F,A])(implicit F: Monad[F]): F[A] = step(free) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(x,f) => x match {
      case Suspend(r) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible")
    }
  }

  trait Translate[F[_], G[_]] {def apply[A](f: F[A]): G[A]}
  type ~>[F[_], G[_]] = Translate[F,G]


  def runFree[F[_], G[_], A](free: Free[F,A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible")
    }

  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
    type FreeG[A] = Free[G,A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G,A] = Suspend { fg(a) }
    }
    runFree(f)(t)(freeMonad[G])
  }
}

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  override def toPar: Par[Option[String]] = Par.lazyUnit(run)

  override def toThunk: () => Option[String] = () => run

  def run: Option[String] =
    try Some(scala.io.StdIn.readLine)
    catch { case e: Exception => None}
}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(line))

  override def toThunk: () => Unit = () => println(line)
}

case class ConvertText(text: Option[String]) extends Console[Option[Double]] {
  override def toPar: Par[Option[Double]] = Par.lazyUnit(run)

  override def toThunk: () => Option[Double] = () => run

  def run: Option[Double] =
    text match {
      case Some(t) => try Some(t.toDouble) catch {case _:Exception => None}
      case None => None
    }
}

object Console {
  type ConsoleIO[A] = Free[Console,A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line : String): ConsoleIO[Unit] =
    Suspend(PrintLine(line))

  def convertText(text : Option[String]): ConsoleIO[Option[Double]] = Suspend(ConvertText(text))

  val f1: ConsoleIO[Option[String]] = for {
    - <- printLn("I can only interact with the console")
    ln <- readLn
  } yield ln

  val consoleToFunction0 =
    new (Console ~>  Function0) {
      override def apply[A](f: Console[A]): () => A = f.toThunk
    }
  val consoleToPar =
    new (Console ~> Par) {
      override def apply[A](f: Console[A]): Par[A] = f.toPar
    }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A,B](a: () => A)(f: A => (() => B)) = () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A,B](a: Par[A])(f: A => Par[B]) = Par.fork(Par.flatMap(a)(f))
  }

  def runConsoleFunction0[A](a: Free[Console,A]): () => A =
    Free.runFree(a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console,A]): Par[A] =
    Free.runFree(a)(consoleToPar)

  def runConsole[A](a: Free[Console,A]): A =
    Free.runTrampoline { Free.translate(a)(new (Console ~> Function0) {
      def apply[A](c: Console[A]) = c.toThunk
    })}


}

