package with_monads

trait ReaderTOps[F[_], A,B] {
    def runA(a: A): F[B]
  }

case class ReaderT[F[_]: Monad, A,B](run: A => F[B]) extends ReaderTOps[F,A,B] {
    def runA(a: A): F[B] = run(a) 
}

object ReaderT {
    implicit def monadForReaderT[F[_]: Monad, A]: Monad[ReaderT[F, A,*]] = new Monad[ReaderT[F, A,*]] {
        def flatMap[B, C](fa: ReaderT[F,A,B])(g: B => ReaderT[F,A,C]): ReaderT[F,A,C] = {
            ReaderT((a) => fa.run(a).flatMap(g(_).run(a)))
        }
        def pure[B](b: B): ReaderT[F, A, B] = ReaderT((a) => Monad[F].pure(b))
    }
}

