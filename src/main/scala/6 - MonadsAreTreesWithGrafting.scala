object MonadsAreTreesWithGrafting1 {
  import TypeClasses2._
  import TypeClasses2.Monad.ops._

  sealed trait Tree[+A]
  final case class Fork[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  case object Nil extends Tree[Nothing]

  val tree1 = Fork(
    Fork(Leaf(10), Nil),
    Fork(Leaf(5), Leaf(3)))

  implicit val treeMonad = new Monad[Tree] {
    override def pure[A](value: A): Tree[A] =
      Leaf(value)

    override def flatMap[A, B](ma: Tree[A])(f: (A) => Tree[B]): Tree[B] =
      ma match {
        case Nil => Nil
        case Leaf(x) => f(x)
        case Fork(x, y) =>
          Fork(flatMap(x)(f), flatMap(y)(f))
      }
  }

  def collect[A](tree: Tree[A]): Vector[A] = tree match {
    case Fork(x, y) => collect(x) ++ collect(y)
    case Leaf(x) => Vector(x)
    case Nil => Vector.empty
  }

  val tree: Tree[Int] = Fork(Leaf(2), Leaf(5))
  val result = for {
    x <- tree
    y <- tree
    z <- tree
  } yield x + y + z

  def run(): Unit = {
    println(getClass.getSimpleName)
    println(result)
    println(collect(result))
    println()
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}