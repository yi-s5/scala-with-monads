package with_monads

// trait IO[A] {
//   def flatMap[A, B](fa: IO[A])(g: A => IO[B]): IO[B]
//   def pure[A](a: A): F[A]
//   def map[A,B](fa: F[A])(g: A => B): F[B] = flatMap(fa)(a => pure(g(a)))
// }
sealed abstract class IO[A]

object IO {
  case class Pure[A](a: A) extends IO[A]
  case class FlatMap[B,A](fa: IO[B], f: B => IO[A]) extends IO[A]
  case class Delay[A](f: () => A) extends IO[A] // Can we pass down Math.random() or just Math.random  ?

  def unsafeRunSync[A](io: IO[A]): A = {
    io match {
      case Delay(f) => f()
      case Pure(a) => a
      case FlatMap(fa, f) => {
        val a = unsafeRunSync(fa)
        unsafeRunSync(f(a))
      }
    }
  }

  def pure[A](a: A): IO[A] = Pure(a)
  def delay[A](f: => A): IO[A] = Delay(() => f)
  //def flatMap[B,A](fa: IO[B], f: B => IO[A]): IO[A] = FlatMap(fa, f)

  implicit val monadForIO: Monad[IO[*]] = new Monad[IO[*]]  {
    def flatMap[A, B](fa: IO[A])(g: A => IO[B]): IO[B] = FlatMap(fa, g)
    def pure[A](a: A): IO[A] = Pure(a)
  }
}

trait IOOps {
  final class IOSyntax[A](ma: IO[A]) {
    // def map[B](g: A => B): IO[B] = implicitly[Monad[IO[*]]].map(ma)(g)
    // def flatMap[B](g: A => IO[B]): IO[B] = implicitly[Monad[IO[*]]].flatMap(ma)(g)
    // def pure(a: A): IO[A] = implicitly[Monad[IO[*]]].pure(a)
    def unsafeRunSync(): A = IO.unsafeRunSync(ma)
  }

  implicit def ioSyntax[A](ma: IO[A]) = new IOSyntax[A](ma)
}

sealed abstract case class ExitCode (exitCode: Int)

object Success extends ExitCode(0)
object Failure extends ExitCode(1)


abstract class IOApp {
  def run(args: Array[String]): IO[ExitCode] 
  def main(args: Array[String]): Unit  = {
    System.exit(run(args).map(_.exitCode).unsafeRunSync())
  }
}