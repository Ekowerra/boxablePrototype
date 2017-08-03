trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.



  def intDouble(rng: RNG): ((Int, Double), RNG)

  def doubleInt(rng: RNG): ((Double, Int), RNG)

  def double3(rng: RNG): ((Double, Double, Double), RNG)

  def ints(counts: Int)(rng: RNG): (List[Int], RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }

  def randomPairSame(rng: RNG): (Int, Int) = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }

  def randomPair(rng: RNG): (Int, Int) = {
    val (i1, rng2) = rng.nextInt
    val (i2, _) = rng2.nextInt
    (i1, i2)
  }





  override def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, r) = rng.nextInt
    val (double, rng2) = Simple.double(r)
    ((int, double), rng2)
  }

  override def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (double, r) = Simple.double(rng)
    val (int, rng2) = r.nextInt
    ((double, int), rng2)

    //    val ((i,d),r) = rng.intDouble(rng)
    //    ((d,i),r)
  }

  override def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = Simple.double(rng)
    val (d2, r2) = Simple.double(r1)
    val (d3, r3) = Simple.double(r2)
    ((d1, d2, d3), r3)
  }

  override def ints(counts: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, r: RNG, rList: List[Int]): (List[Int], RNG) = {
      if (n > 0) {
        val (i, r2) = r.nextInt
        go(n - 1, r2, i :: rList)
      } else (rList, r)
    }

    go(counts, rng, List.empty[Int])
  }
}
object Simple {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng2)
  }


  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue + 1).toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](lra: List[Rand[A]]): Rand[List[A]] = {
    def go(rList: List[Rand[A]], res: List[A], r: RNG): (List[A], RNG) = {
      rList match {
        case Nil => (res.reverse, r)
        case h :: t =>
          val (a, r1) = h(r)
          go(t, a :: res, r1)
      }
    }

    r => go(lra, List.empty[A], r)

    //      lra.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def intsViaS(counts: Int): Rand[List[Int]] =
    sequence(List.fill(counts)(_.nextInt))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    r => {
      val (a, r1) = f(r)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i =>
      if (i + (n - 1) - (i % n) >= 0)
        unit(i % n)
      else
        nonNegativeLessThan(n)
    )

  }

  def mapViaFM[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map2ViaFM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  //    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)



}


val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt
rng.randomPair(rng)
rng.randomPair(rng2)
rng.randomPairSame(rng2)

Simple.nonNegativeInt(rng)
val rng4 = Simple.double(rng)._2
Simple.double(rng3)

rng.double3(rng)

rng.ints(5)(rng)
Simple.intsViaS(5)(rng)

Simple.double(rng)
Simple.doubleViaMap

Simple.nonNegativeLessThan(59)(rng)

val zero = Simple.rollDie(SimpleRNG(41))._1



