case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(x => State.unit(f(x)))

  def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => s2.map(b => f(a, b)))


}

object State {

  def modify[S](f : S => S): State[S,Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S,S] = State(s => (s,s))

  def set[S](s :S): State[S,Unit] = State(_ => ((),s))

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](lsa: List[State[S, A]]): State[S, List[A]] =
    lsa.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))
}


sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def input(in: Input): State[Machine, (Int, Int)] = {
      State(m => {
        (in,m) match {
          case (Coin, Machine(true, ca, co)) if ca > 0 => ((co +1, ca),Machine(false, ca, co + 1))
          case (Turn, Machine(false, ca, co)) if ca > 0 => ((co,ca-1),Machine(true, ca - 1, co))
          case (Turn, Machine(true, ca, co)) if ca > 0 => ((co,ca),Machine(true, ca, co))
          case (Coin, Machine(false, ca, co)) if ca > 0 => ((co,ca),Machine(false, ca, co))
          case (_, Machine(locked, 0, co)) => ((co,0),Machine(locked, 0, co))
        }
      })
    }


    inputs.foldLeft(State[Machine, (Int, Int)](s =>
      ((s.coins, s.candies),s)))((acc,f) => acc.flatMap(_ => input(f)))
  }

}

val inputs = List[Input](Coin,Turn,Turn,Coin,Coin,Turn,Coin)
Machine.simulateMachine(inputs).run(Machine(true, 2, 2))



object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
    s <- State.get
  } yield (s.coins, s.candies)
}

val inputs2 = List[Input](Coin,Turn,Turn,Coin)
Candy.simulateMachine(inputs2).run(Machine(true, 2, 2))
