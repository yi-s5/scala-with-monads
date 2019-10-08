package with_monads

trait WriterOps[A] {
  def value(value: A): Writer[A]
  def tell(entries: List[String]): Writer[A]
}

case class Writer[A](log: List[String], value: A) { 
  
}