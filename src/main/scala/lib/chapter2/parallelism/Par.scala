package lib.chapter2

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Par {
type Par[A] = ExecutorService => Future[A]
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(evenIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))


  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A,B,C](p1: Par[A], p2: Par[B])(f : (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = p1(es)
      val bf = p2(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map3[A,B,C,D](p1 : Par[A], p2 : Par[B], p3: Par[C])(f: (A,B,C) => D)=
    map2(map2(p1,p2)((a,b) => f.curried(a)(b)(_ : C)), p3)((g, c) => g(c))

  def map4[A,B,C,D,E](p1 : Par[A], p2 : Par[B], p3: Par[C], p4: Par[D])(f: (A,B,C,D) => E)=
    map2(map3(p1,p2,p3)((a,b,c) => f.curried(a)(b)(c)(_ : D)), p4)((g, d) => g(d))

  def map5[A,B,C,D,E,F](p1 : Par[A], p2 : Par[B], p3: Par[C], p4: Par[D], p5: Par[E])(f: (A,B,C,D,E) => F)=
    map2(map4(p1,p2,p3,p4)((a,b,c,d) => f.curried(a)(b)(c)(d)(_ : E)), p5)((g,e) => g(e))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](ps: List[A])(f : A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  def parFilter[A](as : List[A])(f: A => Boolean) : Par[List[A]] = {
    /*def ano(l : List[A]): List[A] = l.filter(f)
    asyncF(ano)(as)*/

    val pars: List[Par[List[A]]] =
      as map asyncF((a: A) => if (f(a)) List(a) else List())
    map(sequence(pars))(_.flatten)
  }

  def parF[A](l : List[A])(f : (A,A) => A): Par[A] = {
    l match {
      case h :: Nil => unit(h)
      case Nil => ???
      case h :: t => {
        val (left,right) = l.splitAt(l.length/2)
        map2(parF(left)(f), parF(right)(f))(f)
      }
    }
  }

/*  /* This version respects timeouts. See `Map2Future` below. */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

  /*
  Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
  We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation
  of pure values won't affect results).
  */
  case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                               f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;val aTime = stop-start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }*/



/*  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
  as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)*/
}


object Examples {
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      println("/2")
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sumViaParF(ints: IndexedSeq[Int]): Par.Par[Int] = Par.parF[Int](ints.toList)(_+_)

  def max(ints: IndexedSeq[Int]): Par.Par[Int] = Par.parF[Int](ints.toList)(_ max _)
}
