package with_monads

trait StateOps[S,A] {
  def runA(s: S): A
}

case class State[S, A](run: (S) => (S, A)) extends StateOps[S,A] {
  def runA(s: S): A = run(s)._2
}

object State {
  implicit def monadForState[S]: Monad[State[S,*]] = new Monad[State[S,*]] {
    def flatMap[A, B](fa: State[S,A])(g: A => State[S,B]): State[S,B] = {
      State(s => {
        val (s2, a) = fa.run(s)
        g(a).run(s2)
      })
    }
    def pure[A](a: A): State[S,A] = State(s => (s,a))

  }
}