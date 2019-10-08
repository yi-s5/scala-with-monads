package with_monads

trait StateOps[S,A] {
  def runA(s: S): A
}

case class State[S,A](run: (S) => (S, A)) {
  
}