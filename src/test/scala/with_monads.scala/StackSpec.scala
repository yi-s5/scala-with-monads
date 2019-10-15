package with_monads.scala

import cats.Monoid
import cats.implicits._

import with_monads._
import with_monads.Writer

import org.scalatest._
import org.scalatest.Matchers._

class StackSpec extends FlatSpec {
    "State" should "handle push and pop on a stack" in {
        val stack = List[Int](1, 2, 3, 4)

        def push(value: Int): State[List[Int], Unit] = State[List[Int], Unit](
            l => (value :: l , Unit)
        )

        def pop: State[List[Int], Int] = State[List[Int], Int](
            l => l match {
                case head :: tl => (tl, head)
                case Nil        => throw new Exception
            }
        )

        def popAndSum: State[List[Int], Int] = for {
            x <- pop
            y <- pop
            _ <- push(x + y)
        } yield(x + y)

        popAndSum.run(List(1,2,3)) should equal((List(3,3), 3))
    }
    
}