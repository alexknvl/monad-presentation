import scala.annotation.tailrec
import scala.language.higherKinds

object MoreTypeClasses {
  import simulacrum.typeclass

  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  @typeclass trait FlatMap[M[_]] {
    def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
  }

  @typeclass trait Monad[F[_]] extends Functor[F] with FlatMap[F] {
    def pure[A](value: A): F[A]

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(x => pure(f(x)))
  }

  sealed abstract class ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }
}

object FreeMonad {
  import MoreTypeClasses._
  import MoreTypeClasses.Monad.ops._

  type FreeC[F[_], A] = Free[Coyoneda[F, ?], A]

  object Free {
    case class Pure[S[_], A](value: A) extends Free[S, A]
    case class Suspend[S[_], A](value: S[Free[S, A]]) extends Free[S, A]
    sealed trait FlatMap[S[_], A] extends Free[S, A] {
      type Pivot
      val left: () => Free[S, Pivot]
      val right: Pivot => Free[S, A]
    }
    def mkFlatMap[S[_], A, B](left0: () => Free[S, A])(right0: A => Free[S, B]): Free[S, B] =
      new FlatMap[S, B] {
        override type Pivot = A
        override val left = left0
        override val right = right0
      }

    def liftF[F[_], A](value: F[A])(implicit F: Functor[F]): Free[F, A] =
      Suspend(F.map(value)(Pure[F, A]))

    def liftFC[F[_], A](value: F[A]): FreeC[F, A] =
      liftF[Coyoneda[F, ?], A](Coyoneda.lift(value))

    def runFC[S[_], M[_], A](fa: FreeC[S, A])(f: S ~> M)(implicit M: Monad[M]): M[A] =
      fa.foldMap[M](new (Coyoneda[S, ?] ~> M) {
        def apply[B](ca: Coyoneda[S, B]): M[B] = M.map(f(ca.fi))(ca.k)
      })

    implicit def freeMonad[S[_]: Functor]: Monad[Free[S, ?]] =
      new Monad[Free[S, ?]] {
        def pure[A](a: A): Free[S, A] = Pure(a)
        override def map[A, B](fa: Free[S, A])(f: A => B): Free[S, B] = fa map f
        def flatMap[A, B](fa: Free[S, A])(f: A => Free[S, B]): Free[S, B] = fa flatMap f
      }

    implicit def freeCMonad[S[_]]: Monad[FreeC[S, ?]] =
      freeMonad[Coyoneda[S, ?]]
  }

  sealed trait Free[S[_], A] {
    import Free._

    final def map[B](f: A => B): Free[S, B] =
      flatMap(a => Pure(f(a)))

    final def flatMap[B](f: A => Free[S, B]): Free[S, B] = this match {
      case a: FlatMap[S, A] => mkFlatMap(a.left)(x => mkFlatMap(() => a.right(x))(f))
      case a => mkFlatMap(() => a)(f)
    }

    @tailrec final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] = this match {
      case Pure(x) => Right(x)
      case Suspend(x) => Left(x)
      case x: FlatMap[S, A] => x.left() match {
        case Pure(a) => x.right(a).resume
        case Suspend(t) =>
          Left(S.map(t)(_ flatMap x.right))
        case y: FlatMap[S, _] =>
          y.left().flatMap(z => y.right(z) flatMap x.right).resume
      }
    }

    final def run(implicit S: Monad[S]) = {
      def run2(t: Free[S, A]): S[A] = t.resume match {
        case Left(s) => S.flatMap(s)(run2)
        case Right(r) => S.pure(r)
      }
      run2(this)
    }

    final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] =
      this.resume match {
        case Left(s) => Monad[M].flatMap(f(s))(_.foldMap(f))
        case Right(r) => Monad[M].pure(r)
      }

    final def mapSuspension[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): Free[T, A] =
      this.resume match {
        case Left(s) => Suspend(f(S.map(s)(((_: Free[S, A]) mapSuspension f))))
        case Right(r) => Pure(r)
      }

    final def compile[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): Free[T, A] =
      mapSuspension(f)
  }

  sealed abstract class Coyoneda[F[_], A] extends Serializable { self =>
    type Pivot
    val fi: F[Pivot]
    val k: Pivot => A

    final def run(implicit F: Functor[F]): F[A] =
      F.map(fi)(k)

    final def map[B](f: A => B): Coyoneda.Aux[F, B, Pivot] =
      Coyoneda(fi)(f compose k)

    final def transform[G[_]](f: F ~> G): Coyoneda.Aux[G, A, Pivot] =
      Coyoneda(f(fi))(k)
  }
  object Coyoneda {
    type Aux[F[_], A, B] =
      Coyoneda[F, A] { type Pivot = B }

    def lift[F[_], A](fa: F[A]): Coyoneda[F, A] =
      apply(fa)(identity[A])

    def apply[F[_], A, B](fa: F[A])(k0: A => B): Aux[F, B, A] =
      new Coyoneda[F, B] {
        type Pivot = A
        val k = k0
        val fi = fa
      }

    implicit def coyonedaFunctor[F[_]]: Functor[Coyoneda[F, ?]] =
      new Functor[Coyoneda[F, ?]] {
        def map[A, B](cfa: Coyoneda[F, A])(f: A => B): Coyoneda[F, B] =
          cfa map f
      }
  }
}

object InterpreterExample {
  import MoreTypeClasses._
  import MoreTypeClasses.Monad.ops._
  import FreeMonad._

  final class World private()
  case class IO[+A](run: World => (World, A)) {
    def unsafePerformIO(): A = {
      val (w, a) = run(null)
      a
    }
  }

  object IO {
    def pure[A](a: A): IO[A] = IO { w => (w, a) }
    def liftIO[A](f: => A): IO[A] = IO { w => (w, f) }

    def putStrLn(s: String): IO[Unit] = liftIO(println(s))
    def readLn: IO[String] = liftIO(scala.io.StdIn.readLine())

    implicit val ioMonad: Monad[IO] = new Monad[IO] {
      override def pure[A](value: A): IO[A] = IO(w => (w, value))
      override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = IO { w0 =>
        val (w1, a) = ma.run(w0)
        f(a).run(w1)
      }
    }
  }

  case class Log[+A](value: A, output: Vector[String])

  object Log {
    def pure[A](value: A): Log[A] = new Log[A](value, Vector.empty)
    def log(value: String): Log[Unit] = new Log[Unit]((), Vector(value))
    def logMany(values: Vector[String]): Log[Unit] = new Log[Unit]((), values)

    implicit def logMonad[M] = new Monad[Log] {
      def pure[A](value: A): Log[A] = Log.pure(value)
      def flatMap[A, B](ma: Log[A])(f: A => Log[B]): Log[B] = {
        val mb = f(ma.value)
        new Log(mb.value, ma.output ++ mb.output)
      }
    }
  }

  sealed trait Operation[A]
  case class Pure[A](a: A) extends Operation[A]
  case class PutStrLn(s: String) extends Operation[Unit]
  case object GetUserName extends Operation[String]

  type Op[A] = Coyoneda[Operation, A]
  object Op {
    def pure[A](a: A): Free[Op, A] =
      Free.liftFC(Pure(a): Operation[A])
    def putStrLn(s: String): Free[Op, Unit] =
      Free.liftFC(PutStrLn(s): Operation[Unit])
    def getUserName: Free[Op, String] =
      Free.liftFC(GetUserName: Operation[String])
  }

  object IOInterpreter extends (Operation ~> IO) {
    def apply[A](op: Operation[A]): IO[A] = op match {
      case Pure(a) => IO.pure(a)
      case PutStrLn(s) => IO.putStrLn(s)
      case GetUserName =>
        for {
          _ <- IO.putStrLn("Enter your name: ")
          name <- IO.readLn
        } yield name
    }
  }

  object LogInterpreter extends (Operation ~> Log) {
    def apply[A](op: Operation[A]): Log[A] = op match {
      case Pure(a) => Log.pure(a)
      case PutStrLn(s) => Log.log(s"Printing $s")
      case GetUserName => for {
        _ <- Log.log("Asked for a user name, returning joe")
      } yield "joe"
    }
  }

  def run(): Unit = {
    val free = for {
      name <- Op.getUserName
      _    <- Op.putStrLn(s"Hello, $name!")
    } yield ()

    val io = Free.runFC(free)(IOInterpreter)
    io.unsafePerformIO()

    val log = Free.runFC(free)(LogInterpreter)
    println(log)
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}