package with_monads.scala

import cats.Monoid
import cats.implicits._

import with_monads._
import with_monads.Writer

import org.scalatest._
import org.scalatest.Matchers._

class WriterSpec extends FlatSpec {

  case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]])

  def tree[A](value: A, left: Option[Tree[A]] = None, right: Option[Tree[A]] = None): Option[Tree[A]] = {
    Some(Tree(value, left, right))
  }

  "Writer" should "correctly log post-order tree traversals" in {
    val t = tree("a",
              tree("b",
                tree("d")),
              tree("c",
                tree("e",
                  tree("g"),
                  tree("h")),
                tree("f"))).get

    def append[A,B](w: Writer[List[A],B], node: Tree[A]) = w.tell(List(node.value))

    // traverse counts the leaves while storing a post-order traversal in the log
    def traverse[A](x: Tree[A]): Option[Writer[List[A], Int]] = {
      val maybeLeft = x.left.flatMap(traverse(_))
      val maybeRight = x.right.flatMap(traverse(_))
      // This could be much easier with a WriterT... why?
      (maybeLeft, maybeRight) match {
        case (None,    None)    => Some(append(Monad[Writer[List[A],*]].pure(1), x))
        case (Some(l), None)    => Some(append(l, x))
        case (None,    Some(r)) => Some(append(r, x))
        case (Some(l), Some(r)) => Some(append(l.flatMap(x => r.value(r.value + x)), x))
      }
    }

    assert(traverse(t).isDefined)
    val writer = traverse(t).get
    writer.value should equal (4)
    println(writer.log)
    writer.log should equal(List("f", "h", "g", "e", "c", "d", "b", "a"))
  }
}