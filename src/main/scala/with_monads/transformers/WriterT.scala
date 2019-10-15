package with_monads.transformers

import with_monads._

import cats.Monoid
import cats.implicits

case class WriterT[F[_]: Monad, L: Monoid,A](value: F[Writer[L,A]]) { 
  def value(newValue: A): WriterT[F,L,A] = WriterT(value.map(_.value(newValue)))
  def tell(entries: L): WriterT[F,L,A] = WriterT(value.map(_.tell(entries)))
}

object WriterT {
  implicit def monadForWriterT[F[_]: Monad, L: Monoid]: Monad[WriterT[F,L,*]] = new Monad[WriterT[F,L,*]] {
    def pure[A](a: A): WriterT[F,L,A] = 
      WriterT(Monad[F].pure(Monad[Writer[L,*]].pure(a)))

    def flatMap[A, B](fa: WriterT[F,L,A])(g: A => WriterT[F,L,B]): WriterT[F,L,B] =
      WriterT(flatMapF(fa)(a => g(a).value))
    
    def flatMapF[A, B](fa: WriterT[F,L,A])(g: A => F[Writer[L,B]]): F[Writer[L,B]] =
      for {
        oldWriter <- fa.value 
        newWriter <- g(oldWriter.value)
      } yield {
        newWriter.tell(oldWriter.log)
      }
  }
}