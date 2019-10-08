package with_monads

trait WriterOps[A] {
  def value(value: A): Writer[A]
  def tell(entries: List[String]): Writer[A]
}

case class Writer[A](log: List[String], value: A) extends WriterOps[A] { 
  def value(newValue: A): Writer[A]          = Writer(log, newValue)
  def tell(entries: List[String]): Writer[A] = Writer(log ++ entries, value)
}

object Writer {
  implicit val monadForWriter: Monad[Writer] = new Monad[Writer] {
    def pure[A](a: A): Writer[A] = Writer(List.empty, a)
    def flatMap[A, B](fa: Writer[A])(g: A => Writer[B]): Writer[B] = 
      g(fa.value).tell(fa.log)
  }
}