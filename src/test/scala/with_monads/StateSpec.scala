package with_monads

import org.scalatest._
import org.scalatest.Matchers._

class StateSpec extends FlatSpec {
  "State" should "allow push and pop operations" in {
    val pop: State[List[Int], Option[Int]] =
      State(list => list match {
        case Nil => (list, None)
        case head :: tl => (tl, Some(head))
      })

    def push(a: Int): State[List[Int], Unit] = 
      State(list => (a :: list, ()))

    val popPopAddAndPush: State[List[Int], Int] = 
      for {
       x <- pop
       y <- pop
       xandy = x.getOrElse(0) + y.getOrElse(0)
       _ <- push(xandy)
      } yield (xandy)

    popPopAddAndPush.run(List(1,2,3)) should equal((List(3,3), 3)) 
  }
}