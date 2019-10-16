package with_monads

import org.scalatest._
import org.scalatest.Matchers._

class StateTSpec extends FlatSpec {
  "StateT" should "Be wrappable by Option" in {
    val pop: StateT[Option, List[Int], Int] =
      StateT(list => list match {
        case Nil => None
        case head :: tl => Some(tl, head)
      })

    def push(a: Int): StateT[Option, List[Int], Unit] = 
      StateT(list => Some((a :: list, ())))

    val popPopAddAndPush: StateT[Option, List[Int], Int] = 
      for {
        x <- pop
        y <- pop
        _ <- push(x+y)
      } yield (x+y)

    popPopAddAndPush.run(List(1,2,3)) should equal(Some(List(3,3), 3))
    popPopAddAndPush.run(List()) should equal(None)
    popPopAddAndPush.run(List(1)) should equal(None)
    popPopAddAndPush.run(List(1,2)) should equal(Some((List(3),3)))
  }

  "StateT" should "Be wrappable by List" in {
    val pop: StateT[List, List[Int], Int] =
      StateT(list => list match {
        case Nil => List()
        case head :: tl => List((tl, head))
      })

    def push(a: Int): StateT[List, List[Int], Unit] = 
      StateT(list => List((a :: list, ())))

    val branchingPop: StateT[List, List[Int], Int] =
    StateT(list => list match {
      case Nil => List()
      case head :: tl => List((tl, head), (tl, head))
    })

    val branchingPopPopAddAndPush: StateT[List, List[Int], Int] = 
      for {
        x <- branchingPop
        y <- pop
        _ <- push(x+y)
      } yield (x+y)

    // TODO: Figure this out
    branchingPopPopAddAndPush.run(List(1,2,3)) should equal(List((List(3,3), 3)))
    branchingPopPopAddAndPush.run(List()) should equal(List())
    branchingPopPopAddAndPush.run(List(1)) should equal(List())
    branchingPopPopAddAndPush.run(List(1,2)) should equal(List((List(3),3)))
  }
}