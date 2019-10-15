package with_monads

import cats.Monoid
import cats.implicits

trait WriterOps[L,A] {
  def value(value: A): Writer[L,A]
  def tell(entries: L): Writer[L,A]
}

case class Writer[L: Monoid,A](log: L, value: A) extends WriterOps[L,A] { 
  def value(newValue: A): Writer[L,A]          = Writer(log, newValue)
  def tell(entries: L): Writer[L,A] = Writer(Monoid[L].combine(log,entries), value)
}

object Writer {
  
  implicit def monadForWriter[L: Monoid]: Monad[Writer[L,*]] = new Monad[Writer[L,*]] {
    def pure[A](a: A): Writer[L,A] = Writer(Monoid[L].empty, a)
    def flatMap[A, B](fa: Writer[L,A])(g: A => Writer[L,B]): Writer[L,B] = 
      g(fa.value).tell(fa.log)
  }
}