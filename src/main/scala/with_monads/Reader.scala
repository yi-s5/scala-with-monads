package with_monads

case class Reader[A,B](run: A => B) {
  
}

object Reader {
  implicit def monadForReader[C]: Monad[Reader[C,*]] = new Monad[Reader[C,*]] {
    def flatMap[A,B](fa: Reader[C,A])(g: A => Reader[C,B]): Reader[C,B] = {
      Reader((c: C) => {
        val a = fa.run(c)
        g(a).run(c)
      })
    }
    def pure[A](a: A): Reader[C,A] = Reader(c => a)
  }
}