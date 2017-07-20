import java.util
import java.util.concurrent._



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

  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = p1(es)
      val bf = p2(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def delay[A](fa : => Par[A]): Par[A] =
    es => fa(es)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    /*def ano(l : List[A]): List[A] = l.filter(f)
    asyncF(ano)(as)*/

    val pars: List[Par[List[A]]] =
      as map asyncF((a: A) => if (f(a)) List(a) else List())
    map(sequence(pars))(_.flatten)
  }

  def parF[A](l: List[A])(f: (A, A) => A): Par[A] = {
    l match {
      case h :: Nil => unit(h)
      case Nil => ???
      case h :: t =>
        val (left, right) = l.splitAt(l.length / 2)
        map2(parF(left)(f), parF(right)(f))(f)
    }
  }


  def equal[A](e : ExecutorService)(p : Par[A], p2 : Par[A]): Boolean =
    p(e).get == p2(e).get()

}

object Examples {
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sumViaParF(ints: IndexedSeq[Int]): Par.Par[Int] = Par.parF[Int](ints.toList)(_+_)
  def max(ints: IndexedSeq[Int]): Par.Par[Int] = Par.parF[Int](ints.toList)(_ max _)
/*  def words(paragraphs : List[String]): Par.Par[Int] = {
    Par.map(Par.parMap(paragraphs)(_.split(" ").length))(x => Par.parF[Int](x)(_+_))
  }*/
}

val ints = IndexedSeq(1,2,3,4,5,6)

Examples.sum(ints)

val es = new ExecutorService {override def submit[T](task: Callable[T]): Future[T] = ???

  override def submit[T](task: Runnable, result: T): Future[T] = ???

  override def submit(task: Runnable): Future[_] = ???

  override def isTerminated: Boolean = ???

  override def invokeAll[T](tasks: util.Collection[_ <: Callable[T]]): util.List[Future[T]] = ???

  override def invokeAll[T](tasks: util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): util.List[Future[T]] = ???

  override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = ???

  override def shutdownNow(): util.List[Runnable] = ???

  override def invokeAny[T](tasks: util.Collection[_ <: Callable[T]]): T = ???

  override def invokeAny[T](tasks: util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): T = ???

  override def shutdown(): Unit = ???

  override def isShutdown: Boolean = ???

  override def execute(command: Runnable): Unit = ???
}
Par.run(es)(Examples.sumViaParF(ints))
Par.run(es)(Examples.max(ints))


val a = Par.lazyUnit(42+1)
val S = Executors.newFixedThreadPool(2)
Par.equal(S)(a,Par.fork(a))