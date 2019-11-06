package with_monads

object Main extends IOApp {
    def run(args: Array[String]): IO[ExitCode] =
        if (args.isEmpty)
            IO.pure(Failure)
        else 
            args(0) match {
                    case "success" => IO.pure(Success)
                    case _         => IO.pure(Failure)
                }
}