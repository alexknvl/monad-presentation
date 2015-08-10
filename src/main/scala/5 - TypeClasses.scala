import scala.language.higherKinds

object TypeClasses1 {
  trait Monad[M[_]] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))

    def pure[A](value: A): M[A]
  }

  object Monad {
    def apply[M[_]](implicit m: Monad[M]): Monad[M] = m

    object ops {
      implicit class MonadOps[M[_], A](value: M[A]) {
        def flatMap[B](f: A => M[B])(implicit M: Monad[M]) = M.flatMap(value)(f)
        def map[B](f: A => B)(implicit M: Monad[M]) = M.map(value)(f)
      }
    }
  }

  sealed trait Maybe[+A] extends Serializable with Product
  final case class Just[+A](value: A) extends Maybe[A]
  case object Nothing extends Maybe[Nothing]

  object Maybe {
    def apply[A](value: A): Maybe[A] = Just(value)
    def empty[A]: Maybe[A] = Nothing
  }

  implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {
    override def pure[A](value: A): Maybe[A] =
      Just(value)
    override def flatMap[A, B](ma: Maybe[A])(f: (A) => Maybe[B]): Maybe[B] =
      ma match {
        case Just(x) => f(x)
        case Nothing => Nothing
      }
  }

  def run(): Unit = {
    import Monad.ops._

    val m1: Maybe[Int] = Just(1)
    val m2 = m1.flatMap { x => if (x > 0) Nothing else Just(2) }

    val x = for {
      a <- Maybe(1)
      b <- Maybe(2)
      c <- m2
      d <- Maybe(a + b + c)
    } yield d

    println(x)
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}

object TypeClasses2 {
  import simulacrum.typeclass

  @typeclass trait Monad[M[_]] {
    // # Monad Laws
    //
    // These two operations must satisfy:
    //   Left identity :
    //      ∀x:A ∀f:A->M[B]
    //      pure(x).flatMap(f) == f(x)
    //
    //   Right identity:
    //      ∀x:M[A]
    //      x.flatMap(identity) == x
    //
    //   Associativity :
    //      ∀x:M[A] ∀f:A->M[B] ∀g:B->M[C]
    //      x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))

    def ap[A, B](ma: M[A])(mf: M[A => B]): M[B] =
      flatMap(mf)(f => map(ma)(f))

    def flatten[A](mma: M[M[A]]): M[A] =
      flatMap(mma)(identity)

    def sequence[A](list: List[M[A]]): M[List[A]] = list match {
      case Nil => pure(Nil)
      case x :: xs => flatMap(x)(h => map(sequence(xs))(t => h :: t))
    }

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = {
      flatMap(ma)(a => map(mb)(b => f(a, b)))
    }
  }

  implicit val optionMonad = new Monad[Option] {
    override def pure[A](value: A): Option[A] =
      Some(value)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma match {
        case Some(x) => f(x)
        case None => None
      }
  }

  def run(): Unit = {
    val M = Monad[Option[?]]

    println(M.sequence(List(Some(1), Some(2), None)))
    println(M.sequence(List(Some(1), Some(2), Some(3))))

    println(M.map2(Some(1), Some(2))(_ + _))
    println(M.map2(Some(1), Option.empty[Int])(_ + _))
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}

object TypeClasses3 {
  import TypeClasses2._
  import TypeClasses2.Monad.ops._

  case class Log[+A](value: A, output: Vector[String])

  object Log {
    def pure[A](value: A): Log[A] = new Log[A](value, Vector.empty)
    def log(value: String): Log[Unit] = new Log[Unit]((), Vector(value))
    def logMany(values: Vector[String]): Log[Unit] = new Log[Unit]((), values)
  }

  implicit def logMonad[M] = new Monad[Log] {
    def pure[A](value: A): Log[A] = Log.pure(value)
    def flatMap[A, B](ma: Log[A])(f: A => Log[B]): Log[B] = {
      val mb = f(ma.value)
      new Log(mb.value, ma.output ++ mb.output)
    }
  }

  def run(): Unit = {
    import Log._

    val result = for {
      x <- pure(10)
      _ <- log(s"x is $x")
      y <- pure(2)
      _ <- log(s"y is $y")
      z <- pure(x + y)
      _ <- log(s"z is $z")
    } yield z

    println(result)
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}