package lib.chapter3.applicative

import lib.chapter2.testing.State
import lib.chapter2.testing.State._
import lib.chapter3.monads.Functor
import lib.chapter3.monoid.{Foldable, Monoid}

trait Applicative[F[_]] extends Functor[F] {


  //def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ (_))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(mapViaApply[A, B => C](fa)(f.curried))(fb)

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit[A => B](f))(fa)


  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(x => x)

  def sequence[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { case ((k, fv), acc) => map2(fv, acc)((v, m) => m + (k -> v)) }

  // (ofa foldLeft unit(Map.empty[K,V])) { case (acc, (k, fv)) =>  map2(acc, fv)((m, v) => m + (k -> v))}

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))


  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(map2[A, B, C => D](fa, fb)(f.curried(_)(_)))(fc)

  // == apply(apply(apply(unit[A => B => C => D](f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(map3[A, B, C, D => E](fa, fb, fc)(f.curried(_)(_)(_)))(fd)

  // == apply(apply(apply(apply(unit[A => B => C => D => E](f.curried))(fa))(fb))(fc))(fd)


  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))
    }
  }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({
      type f[x] = Either[E, x]
    })#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] =
        ma match {
          case Left(e) => Left(e)
          case Right(a) => f(a)
        }
    }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({
      type f[x] = Validation[E, x]
    })#f] {
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Failure(e1, t1), Failure(e2, t2)) => Failure(e1, t1 ++ t2 :+ e2)
          case (Success(a), Success(b)) => Success(f(a, b))
          case (e@Failure(_, _), _) => e // == case (Failure(e,t), _) => Failure(e,t)
          case (_, e@Failure(_, _)) => e // == case (_, Failure(e,t)) => Failure(e,t)
        }

      override def unit[A](a: => A): Validation[E, A] = Success(a)
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

}


trait Traverse[F[_]] extends Functor[F] with Foldable[F]{ self =>
  def traverse[M[_]:Applicative,A,B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence[M,B](map(fa)(f))
  def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): A = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a,b) => ((),f(b,a)))._2

  def zip[A,B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  def fuse[G[_], H[_], A,B](fa : F[A])(f:A => G[B], g: A => H[B])
                                      (G:Applicative[G])(H: Applicative[H]):(G[F[B]],H[F[B]]) =
    //(traverse(fa)(f),traverse(fa)(g))
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }

  def composeM[M[_], N[_]](F: Monad[M], G: Monad[N], T: Traverse[N]): Monad[({type f[x] = M[N[x]]})#f] =
    new Monad[({type f[x] = M[N[x]]})#f] {
      override def unit[A](a: => A): M[N[A]] =
        F.unit(G.unit(a))

      override def flatMap[A,B](mna: M[N[A]])(f: A => M[N[B]]): M[N[B]] =
        F.flatMap(mna)(na => F.map(T.traverse(na)(f))(G.join))
    }



}

object Traverse {
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_],A,B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None    => G.unit(None)
      }

  }

  case class Tree[+A](head: A, tail: List[A])

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(f))(Tree(_, _))
  }
}
