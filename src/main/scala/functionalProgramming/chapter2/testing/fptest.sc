import Prop.{Falsified, Passed}
import functionalProgramming.chapter2.testing.{RNG, State, Stream}


case class Prop(run: (Prop.MaxSize, Prop.TestCase, RNG) => Prop.Result) {

  def check: Prop.Result = ???

  def &&(p: Prop): Prop = Prop((max, n, rng) => run(max, n, rng) match {
    case Passed => p.run(max, n, rng)
    case Falsified(f, s) => Falsified(f, s) // case x => x
  })

  def ||(p: Prop): Prop = Prop((max, n, rng) => run(max, n, rng) match {
    case Passed => Passed
    case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
  })

  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCase = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: Prop.FailedCase, successes: Prop.SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }


  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }


}

case class Gen[+A](sample: State[RNG, A]) {

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(State.sequence(List(State(RNG.nonNegativeInt), State(RNG.nonNegativeInt))).map(l => (l.head, l(1))))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  /*  def listOfNViaFM(n: Int, g : Gen[A]): Gen[List[A]] =
      flatMap(a => Gen(State(s => (List.fill(n)(a),s))))*/


  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfNViaFM(size: Gen[Int]): Gen[List[A]] =
    size flatMap listOfN

  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State(rng => (a, rng)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  /*  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      val down = if (g1._2.abs <= g2._2.abs) g1 else g2
      val up = if (g1._2.abs > g2._2.abs) g1 else g2
      val tot = g1._2.abs + g2._2.abs
      Gen(State(RNG._double).map(x => x.abs % tot)).flatMap(y => if (y <= down._2.abs) down._1 else up._1)

    }*/

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN( n max 1))



}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen { forSize(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      forSize(n) flatMap { f(_).forSize(n) }
    }
    SGen(g2)
  }

}



println("hello")
val smallInt = Gen.choose(-10,10)

val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns => {
  val max = ns.max
  !ns.exists(_ > max)
}}

val sortedProp : Prop = Prop.forAll(Gen.listOf(smallInt)) {
  ns => {
    val sort = ns.sorted
    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
    sort.isEmpty || sort.tail.isEmpty || !sort.zip(sort.tail).exists {
      case (a, b) => a > b
    } && !ns.exists(!sort.contains(_)) && !sort.exists(!ns.contains(_))
  }
}

println(Prop.run(maxProp))
println(Prop.run(sortedProp))
