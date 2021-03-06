import scala.util.{Failure, Success, Try}

object TryAndError1 {
  sealed trait Result[+A]
  final case class Okay[A](value: A) extends Result[A]
  final case class Error(message: String) extends Result[Nothing]

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Result[Account] = {
      name match {
        case "Andrew" => Okay(Account(1))
        case "Alex" => Okay(Account(2))
        case "Nick" => Okay(Account(3))
        case "Iren" => Okay(Account(4))
        case _ => Error(s"Unknown account name $name.")
      }
    }

    def getBalance(account: Account): Result[Money] = {
      account.id match {
        case 1 => Okay(1000)
        case 2 => Okay(100)
        case _ => Error(s"No balance associated with account ${account.id}.")
      }
    }

    def qualifiedAmount(amount: Money): Result[Money] = {
      if (amount >= 1000) Okay(1000)
      else Error(s"Insufficient funds for a loan, current balance is $amount.")
    }
  }

  def getLoan(name: String): Result[Money] = {
    Api.getAccount(name) match {
      case Okay(account) =>
        Api.getBalance(account) match {
          case Okay(balance) =>
            Api.qualifiedAmount(balance) match {
              case Okay(x) => Okay(x)
              case Error(e) => Error(e)
            }
          case Error(e) => Error(e)
        }
      case Error(e) => Error(e)
    }
  }

  def run(): Unit = {
    for (name <- List("Alex", "Andrew", "Nick", "Iren", "Fake")) {
      println(s"$name -> ${getLoan(name)}")
    }
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}

object TryAndError2 {
  sealed trait Result[+A] {
    def flatMap[B](f: A => Result[B]): Result[B] = this match {
      case Okay(x) => f(x)
      case Error(e) => Error(e)
    }

    def map[B](f: A => B): Result[B] = this match {
      case Okay(x) => Okay(f(x))
      case Error(e) => Error(e)
    }
  }
  final case class Okay[A](value: A) extends Result[A]
  final case class Error(message: String) extends Result[Nothing]

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Result[Account] = {
      name match {
        case "Andrew" => Okay(Account(1))
        case "Alex" => Okay(Account(2))
        case "Nick" => Okay(Account(3))
        case "Iren" => Okay(Account(4))
        case _ => Error(s"Unknown account name $name.")
      }
    }

    def getBalance(account: Account): Result[Money] = {
      account.id match {
        case 1 => Okay(1000)
        case 2 => Okay(100)
        case _ => Error(s"No balance associated with account ${account.id}.")
      }
    }

    def qualifiedAmount(amount: Money): Result[Money] = {
      if (amount >= 1000) Okay(1000)
      else Error(s"Insufficient funds for a loan, current balance is $amount.")
    }
  }

  def getLoan1(name: String): Result[Money] = {
    Api.getAccount(name)
      .flatMap(account => Api.getBalance(account))
      .flatMap(balance => Api.qualifiedAmount(balance))
  }

  def getLoan2(name: String): Result[Money] = for {
    account <- Api.getAccount(name)
    balance <- Api.getBalance(account)
    loan <- Api.qualifiedAmount(balance)
  } yield loan

  def run(): Unit = {
    for (name <- List("Alex", "Andrew", "Nick", "Iren", "Fake")) {
      println(s"$name -> ${getLoan1(name)}, ${getLoan2(name)}")
    }
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}

object TryAndError3 {
  sealed trait Result[+A] {
    def flatMap[B](f: A => Result[B]): Result[B] = this match {
      case Okay(x) => f(x)
      case Error(e) => Error(e)
    }

    def map[B](f: A => B): Result[B] = this match {
      case Okay(x) => Okay(f(x))
      case Error(e) => Error(e)
    }
  }
  final case class Okay[A](value: A) extends Result[A]
  final case class Error(message: String) extends Result[Nothing]

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Result[Account] = {
      name match {
        case "Andrew" => Okay(Account(1))
        case "Alex" => Okay(Account(2))
        case "Nick" => Okay(Account(3))
        case "Iren" => Okay(Account(4))
        case _ => Error(s"Unknown account name $name.")
      }
    }

    def getBalance(account: Account): Result[Money] = {
      account.id match {
        case 1 => Okay(1000)
        case 2 => Okay(100)
        case _ => Error(s"No balance associated with account ${account.id}.")
      }
    }

    def qualifiedAmount(amount: Money): Result[Money] = {
      if (amount >= 1000) Okay(1000)
      else Error(s"Insufficient funds for a loan, current balance is $amount.")
    }
  }

  def getLoan1(name: String): Result[Money] = {
    Api.getAccount(name)
      .flatMap(account => Api.getBalance(account))
      .flatMap(balance => Api.qualifiedAmount(balance))
  }

  def getLoan2(name: String): Result[Money] = for {
    account <- Api.getAccount(name)
    balance <- Api.getBalance(account)
    loan <- Api.qualifiedAmount(balance)
  } yield loan

  def run(): Unit = {
    for (name <- List("Alex", "Andrew", "Nick", "Iren", "Fake")) {
      println(s"$name -> ${getLoan1(name)}, ${getLoan2(name)}")
    }
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}

object TryAndError4 {
  type Result[+A] = Either[String, A]

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Result[Account] = {
      name match {
        case "Andrew" => Right(Account(1))
        case "Alex" => Right(Account(2))
        case "Nick" => Right(Account(3))
        case "Iren" => Right(Account(4))
        case _ => Left(s"Unknown account name $name.")
      }
    }

    def getBalance(account: Account): Result[Money] = {
      account.id match {
        case 1 => Right(1000)
        case 2 => Right(100)
        case _ => Left(s"No balance associated with account ${account.id}.")
      }
    }

    def qualifiedAmount(amount: Money): Result[Money] = {
      if (amount >= 1000) Right(1000)
      else Left(s"Insufficient funds for a loan, current balance is $amount.")
    }
  }

  def getLoan1(name: String): Result[Money] = {
    Api.getAccount(name)
      .right.flatMap(account => Api.getBalance(account))
      .right.flatMap(balance => Api.qualifiedAmount(balance))
  }

  def getLoan2(name: String): Result[Money] = for {
    account <- Api.getAccount(name).right
    balance <- Api.getBalance(account).right
    loan <- Api.qualifiedAmount(balance).right
  } yield loan

  def run(): Unit = {
    for (name <- List("Alex", "Andrew", "Nick", "Iren", "Fake")) {
      println(s"$name -> ${getLoan1(name)}, ${getLoan2(name)}")
    }
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}

object TryAndError5 {
  case class Account(id: Int)
  type Money = BigInt

  class ApiException(message: String) extends RuntimeException(message)

  object Api {
    def getAccount(name: String): Try[Account] = {
      name match {
        case "Andrew" => Success(Account(1))
        case "Alex" => Success(Account(2))
        case "Nick" => Success(Account(3))
        case "Iren" => Success(Account(4))
        case _ => Failure(new ApiException(s"Unknown account name $name."))
      }
    }

    def getBalance(account: Account): Try[Money] = {
      account.id match {
        case 1 => Success(1000)
        case 2 => Success(100)
        case _ => Failure(new ApiException(s"No balance associated with account ${account.id}."))
      }
    }

    def qualifiedAmount(amount: Money): Try[Money] = {
      if (amount >= 1000) Success(1000)
      else Failure(new ApiException(s"Insufficient funds for a loan, current balance is $amount."))
    }
  }

  def getLoan1(name: String): Try[Money] = {
    Api.getAccount(name)
      .flatMap(account => Api.getBalance(account))
      .flatMap(balance => Api.qualifiedAmount(balance))
  }

  def getLoan2(name: String): Try[Money] = for {
    account <- Api.getAccount(name)
    balance <- Api.getBalance(account)
    loan <- Api.qualifiedAmount(balance)
  } yield loan

  def run(): Unit = {
    for (name <- List("Alex", "Andrew", "Nick", "Iren", "Fake")) {
      println(s"$name -> ${getLoan1(name)}, ${getLoan2(name)}")
    }
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}