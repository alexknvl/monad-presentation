object TheInfamousIOMonad1 {
  import TypeClasses2._
  import TypeClasses2.Monad.ops._

  final class World private()
  case class IO[+A](run: World => (World, A)) {
    def unsafePerformIO(): Unit = run(null)
  }

  object IO {
    def liftIO[A](f: => A): IO[A] = IO { w => (w, f) }

    implicit val ioMonad: Monad[IO] = new Monad[IO] {
      override def pure[A](value: A): IO[A] = IO(w => (w, value))
      override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = IO { w0 =>
        val (w1, a) = ma.run(w0)
        f(a).run(w1)
      }
    }
  }

  def putStrLn(s: String): IO[Unit] = IO.liftIO(println(s))
  def readLn: IO[String] = IO.liftIO(scala.io.StdIn.readLine())

  def run1(): IO[Unit] = IO { w0 =>
    val (w1, _) = putStrLn("What's your name?").run(w0)
    val (w2, name) = readLn.run(w1)
    val (w3, _) = putStrLn(s"Hello, $name").run(w2)
    (w3, ())
  }

  def run2(): IO[Unit] = for {
    _ <- putStrLn("What's your name?")
    name <- readLn
    _ <- putStrLn(s"Hello, $name")
  } yield ()

  def run(): Unit = {
    val r1 = run1()
    val r2 = run2()
    val list = List(run1(), putStrLn("And now, run2"), run2(), putStrLn("Goodbye."))
    val program = Monad[IO].sequence(list)

    println("Nothing has been printed yet!")
    program.unsafePerformIO()
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}