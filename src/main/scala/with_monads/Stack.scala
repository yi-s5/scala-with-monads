package with_monads

// import cats.implicits._

class Stack { 
    def push(value: Int): State[List[Int], Unit] = State[List[Int], Unit](
        l => (value :: l , Unit)
    )

    def pop: State[List[Int], Int] = State[List[Int], Int](
        l => l match {
            case head :: tl => (tl, head)
            case Nil        => throw new Exception
        }
    )

    def ops = for {
        x <- pop
        y <- pop
        _ <- push(x + y)
    } yield(x + y)
}