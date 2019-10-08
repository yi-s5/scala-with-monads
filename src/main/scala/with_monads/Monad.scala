package with_monads
 
trait Monad[F[_]] {
  def map[A, B](fa: F[A])(g: A => B): F[B]
  def flatMap[A, B](fa: F[A])(g: A => F[B]): F[B]
  def pure[A](a: A): F[A]
}

object Monad {
  implicit def monadForOption: Monad[Option] = new Monad[Option] {
    def flatMap[A,B](fa: Option[A])(g: A => Option[B]): Option[B] = {
      fa match {
        case None    => None
        case Some(a) => g(a)
      }
    }
    def map[A,B](fa: Option[A])(g: A => B): Option[B] = {
      fa match {
        case None => None
        case Some(a) => Some(g(a))
      }
    }
    def pure[A](a: A): Option[A] = Some(a)
  }

  implicit def monadForList: Monad[List] = new Monad[List] {
    def flatMap[A,B](fa: List[A])(g: A => List[B]): List[B] = {
      fa match {
        case Nil => Nil
        case hd :: tail => g(hd) ++ flatMap(tail)(g)
      }
    }
    def map[A,B](fa: List[A])(g: A => B): List[B] = {
      fa match {
        case Nil => Nil
        case hd :: tail => g(hd) :: map(tail)(g)
      }
    }
    def pure[A](a: A): List[A] = List(a)
  }

  implicit def monadForEither[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    def flatMap[A,B](fa: Either[E,A])(g: A => Either[E,B]): Either[E,B] = {
      fa match {
        case Left(e) => Left(e)
        case Right(a) => g(a)
      }
    }
    def map[A,B](fa: Either[E,A])(g: A => B): Either[E,B] = {
      fa match {
        case Left(e) => Left(e)
        case Right(a) => Right(g(a))
      }
    }
    def pure[A](a: A): Either[E,A] = Right(a)
  }
}