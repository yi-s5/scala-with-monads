package with_monads

trait StateTOps[F[_], S,A] {
  def runA(s: S): F[A]
}

case class StateT[F[_]: Monad, S,A](run: S => F[(S,A)]) extends StateTOps[F,S,A] {
  def runA(s: S): F[A] = run(s).map(_._2)
}

object StateT {
  implicit def monadForStateT[F[_]: Monad, S]: Monad[StateT[F, S,*]] = new Monad[StateT[F, S,*]] {
    def flatMap[A,B](fa: StateT[F,S,A])(g: A => StateT[F,S,B]): StateT[F,S,B] = {
      StateT((s) => 
        fa.run(s).flatMap({ 
          case (s,a) => g(a).run(s)
        }))
    }
    def pure[A](a: A): StateT[F, S,A] = StateT((s) => Monad[F].pure((s,a)))
  }
  def set[S](s: S): State[S,Unit] = 
    State(prev => (s,()))
  def get[S]: State[S,S] = 
    State(s => (s,s))
  def modify[S](f: S => S): State[S,Unit] = 
    for {
      old <- get[S]
      _   <- set[S](f(old))
    } yield (())
}