package with_monads

import cats.Semigroup
import cats.kernel.Monoid

trait WriterOps[L, A] {
  def value(value: A): Writer[L, A]
  def tell(entries: L): Writer[L, A]
}

case class Writer[L: Semigroup, A](log: L , value: A) extends WriterOps[L, A] { 
  def value(newValue: A): Writer[L, A] = Writer(log, newValue)
  def tell(entries: L): Writer[L, A]   = Writer(Semigroup.combine(log, entries), value)
}

object Writer {
  implicit def monadForWriter[L: Semigroup](implicit ev: Monoid[L]): Monad[Writer[L, *]] = new Monad[Writer[L, *]] {
    def pure[A](a: A): Writer[L, A] = Writer(Monoid.empty, a)
    def flatMap[A, B](fa: Writer[L, A])(g: A => Writer[L, B]): Writer[L, B] = 
      g(fa.value).tell(fa.log)
  }
}