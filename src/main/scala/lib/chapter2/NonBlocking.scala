package lib.chapter2

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object NonBlocking {

  trait Future[+A] {
    private[chapter2] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): Par[A] =
      es => (cb: A => Unit) => cb(a)

    def fork[A](a: => Par[A]): Par[A] =
      es => (cb: A => Unit) => eval(es)(a(es)(cb))

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call = r
      })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => (cb: C => Unit) => {
        var ar: Option[A] = None
        var br: Option[B] = None
        // this implementation is a little too liberal in forking of threads -
        // it forks a new logical thread for the actor and for stack-safety,
        // forks evaluation of the callback `cb`
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(cb(f(a, br.get)))
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }


    // specialized version of `map`
    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => (cb: B => Unit) => p(es)(a => eval(es) {
        cb(f(a))
      })

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => (cb: A => Unit) => p(es) { b =>
        if (b) eval(es) {
          t(es)(cb)
        }
        else eval(es) {
          f(es)(cb)
        }
      }

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => (cb: A => Unit) => n(es) { index =>
        eval(es) {
          choices(index)(es)(cb)
        }
      }

    def choiceViaCN[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN[A](map(p)(x => if (x) 1 else 0))(List[Par[A]](f, t))


    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => (cb: V => Unit) => key(es) { k =>
        choices(k)(es)(cb)
      }

    def choiceNViaCM[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      choiceMap(n)(choices.zipWithIndex.map(x => (x._2, x._1)).toMap)

    def choiceViaCM[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceMap(p)(Map(true -> t, false -> f))


    def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      es => (cb: B => Unit) => pa(es) { a =>
        choices(a)(es)(cb)
      }

    def choiceViaChooser[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      flatMap(p)(bool => if (bool) t else f)

    def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(n)(x => choices(x))

    def join[A](a: Par[Par[A]]): Par[A] =
      es => (cb: A => Unit) => a(es) {
        pa => eval(es) { pa(es)(cb) }
      }

    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(map(p)(f))

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)


  }

}