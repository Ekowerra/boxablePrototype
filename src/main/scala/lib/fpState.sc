case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s => {
      val (a,s1) = run(s)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S,B] =
    flatMap(x => State.unit(f(x)))

  def map2[B,C](s2 : State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => s2.map(b => f(a,b)))

}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S,A](lsa : List[State[S,A]]): State[S,List[A]] =
    lsa.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))
  // lsa.foldRight(State[S,List[A]](s => (List[A](),s)))((f, acc) => f.map2(acc)(_ :: _))

  //  def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
  //    actions match {
  //      case Nil => (acc.reverse,s)
  //      case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
  //    }
  //  State((s: S) => go(s,sas,List())
}