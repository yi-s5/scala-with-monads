package with_monads.transformers

import cats._
import cats.implicits._
import cats.{Monad => CMonad} // because we do not want cats monads and their silly tailRecM... yet.
import with_monads.Monad

case class OptionT[F[_]: CMonad, A](value: F[Option[A]]) {
  def map[B](f: A => B): OptionT[F, B] =
    OptionT(value.map(_ match {
      case None => None
      case Some(a) => Some(f(a))
    }))

  def flatMap[B](f: A => OptionT[F, B]): OptionT[F,B] ={
    OptionT(value.flatMap(option => option match {
      case None => CMonad[F].pure(None)
      case Some(a) => f(a).value
    }))
  }
}

object OptionT {
  implicit def monadForOptionT[F[_]: CMonad]: Monad[OptionT[F, *]] = new Monad[OptionT[F, *]] {
    def flatMap[A, B](fa: OptionT[F,A])(f: A => OptionT[F,B]): OptionT[F,B] = fa.flatMap(f)
    def pure[A](x: A): OptionT[F,A] = OptionT(CMonad[F].pure(Some(x)))
  }
}