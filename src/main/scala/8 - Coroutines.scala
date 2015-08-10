import scala.annotation.tailrec

object CoroutineV1 {
  import TypeClasses2._
  import TypeClasses2.Monad.ops._

  final class World private()
  case class IO[+A](run: World => (World, A)) {
    def unsafePerformIO(): Unit = run(null)
  }

  implicit val ioMonad: Monad[IO] = new Monad[IO] {
    override def pure[A](value: A): IO[A] = IO(w => (w, value))
    override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = IO { w0 =>
      val (w1, a) = ma.run(w0)
      f(a).run(w1)
    }
  }

  object IO {
    def pure[A](value: A): IO[A] = IO { w => (w, value) }
    def liftIO[A](f: () => A): IO[A] = IO { w => (w, f()) }
  }

  sealed trait Coroutine[+A] {
    final def map[B](f: A => B): Coroutine[B] =
      flatMap(a => Pure(f(a)))

    final def flatMap[B](f: A => Coroutine[B]): Coroutine[B] = this match {
      case a: FlatMap[A] => mkFlatMap(a.left)(x => mkFlatMap(() => a.right(x))(f))
      case a => mkFlatMap(() => a)(f)
    }

    @tailrec final def step: Either[IO[Coroutine[A]], A] = this match {
      case Pure(x) => Right(x)
      case Suspend(x) => Left(x)
      case x: FlatMap[A] => x.left() match {
        case Pure(a) => x.right(a).step
        case Suspend(t) =>
          Left(t.map(_ flatMap x.right))
        case y: FlatMap[_] =>
          y.left().flatMap(z => y.right(z) flatMap x.right).step
      }
    }

    final def run = {
      def run2(t: Coroutine[A]): IO[A] = t.step match {
        case Left(s) => Monad[IO].flatMap(s)(run2)
        case Right(r) => IO.pure(r)
      }
      run2(this)
    }
  }

  object Coroutine {
    def fork[A, B](ca: Coroutine[A], cb: Coroutine[B]): Coroutine[(A, B)] = Suspend(
      (ca.step, cb.step) match {
        case (Right(a), Right(b)) => IO.pure(Pure((a, b)))
        case (Right(a), Left(ioB)) => ioB.map(_.map((a, _)))
        case (Left(ioA), Right(b)) => ioA.map(_.map((_, b)))
        case (Left(ioA), Left(ioB)) => for {
          a <- ioA
          b <- ioB
        } yield fork(a, b)
      }
    )
  }
  case class Pure[A](value: A) extends Coroutine[A]
  case class Suspend[A](value: IO[Coroutine[A]]) extends Coroutine[A]
  sealed trait FlatMap[A] extends Coroutine[A] {
    type Pivot
    val left: () => Coroutine[Pivot]
    val right: Pivot => Coroutine[A]
  }
  def mkFlatMap[A, B](left0: () => Coroutine[A])(right0: A => Coroutine[B]): Coroutine[B] =
    new FlatMap[B] {
      override type Pivot = A
      override val left = left0
      override val right = right0
    }

  def putStrLn(s: String): Coroutine[Unit] =
    Suspend(IO.liftIO(() => println(s)).map(f => Pure(f)))

  def run(): Unit = {
    val first = for {
      _ <- putStrLn("1 - 1")
      _ <- putStrLn("1 - 2")
      _ <- putStrLn("1 - 3")
    } yield ()
    val second = for {
      _ <- putStrLn("2 - 1")
      _ <- putStrLn("2 - 2")
      _ <- putStrLn("2 - 3")
    } yield ()
    println("Nothing has been printed yet!")

    val s = Coroutine.fork(first, second)
    println("Nothing happened yet.")

    s.run.unsafePerformIO()
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}