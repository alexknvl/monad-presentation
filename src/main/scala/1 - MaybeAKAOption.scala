object MaybeAKAOption0 {
  case class Account(id: Int)

  object Api {
    def getAccount(name: String): Account = {
      name match {
        case "Andrew" => Account(1)
        case "Alex" => Account(2)
        case "Nick" => Account(3)
        case "Iren" => Account(4)
      }
    }

    def getBalance(account: Account): Int = {
      account.id match {
        case 1 => 1000
        case 2 => 100
      }
    }

    def qualifiedAmount(amount: Int): Int = {
      if (amount >= 1000) 1000
      else 0
    }
  }

  def getLoan(name: String): Int = {
    val account = Api.getAccount(name)
    val balance = Api.getBalance(account)
    val loan = Api.qualifiedAmount(balance)

    loan
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

object MaybeAKAOption1 {
  case class Account(id: Int)

  object Api {
    def getAccount(name: String): Account = {
      name match {
        case "Andrew" => Account(1)
        case "Alex" => Account(2)
        case "Nick" => Account(3)
        case "Iren" => Account(4)
        case _ => null
      }
    }

    def getBalance(account: Account): Int = {
      account.id match {
        case 1 => 1000
        case 2 => 100
        case _ => -1
      }
    }

    def qualifiedAmount(amount: Int): Int = {
      if (amount >= 1000) 1000
      else -1
    }
  }

  def getLoan(name: String): Int = {
    val account = Api.getAccount(name)
    if (account == null) -1
    else {
      val balance = Api.getBalance(account)
      if (balance == -1) -1
      else {
        val loan = Api.qualifiedAmount(balance)
        loan
      }
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

object MaybeAKAOption2 {
  sealed trait Maybe[+A]
  final case class Just[A](value: A) extends Maybe[A]
  case object Nothing extends Maybe[Nothing]

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Maybe[Account] = {
      name match {
        case "Andrew" => Just(Account(1))
        case "Alex" => Just(Account(2))
        case "Nick" => Just(Account(3))
        case "Iren" => Just(Account(4))
        case _ => Nothing
      }
    }

    def getBalance(account: Account): Maybe[Money] = {
      account.id match {
        case 1 => Just(1000)
        case 2 => Just(100)
        case _ => Nothing
      }
    }

    def qualifiedAmount(amount: Money): Maybe[Money] = {
      if (amount >= 1000) Just(1000)
      else Nothing
    }
  }

  def getLoan(name: String): Maybe[Money] = {
    Api.getAccount(name) match {
      case Just(account) =>
        Api.getBalance(account) match {
          case Just(balance) =>
            Api.qualifiedAmount(balance) match {
              case Just(x) => Just(x)
              case Nothing => Nothing
            }
          case Nothing => Nothing
        }
      case Nothing => Nothing
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

object MaybeAKAOption3 {
  sealed trait Maybe[+A] {
    def bind[B](f: A => Maybe[B]): Maybe[B] =
      this match {
        case Just(x) => f(x)
        case Nothing => Nothing
      }
  }
  final case class Just[A](value: A) extends Maybe[A]
  case object Nothing extends Maybe[Nothing]

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Maybe[Account] = {
      name match {
        case "Andrew" => Just(Account(1))
        case "Alex" => Just(Account(2))
        case "Nick" => Just(Account(3))
        case "Iren" => Just(Account(4))
        case _ => Nothing
      }
    }

    def getBalance(account: Account): Maybe[Money] = {
      account.id match {
        case 1 => Just(1000)
        case 2 => Just(100)
        case _ => Nothing
      }
    }

    def qualifiedAmount(amount: Money): Maybe[Money] = {
      if (amount >= 1000) Just(1000)
      else Nothing
    }
  }

  def bind[A, B](ma: Maybe[A])(f: A => Maybe[B]): Maybe[B] = {
    ma match {
      case Just(x) => f(x)
      case Nothing => Nothing
    }
  }

  def getLoan1(name: String): Maybe[Money] = {
    val account = Api.getAccount(name)
    val balance = bind(account)(Api.getBalance)
    val loan = bind(balance)(Api.qualifiedAmount)
    loan
  }

  def getLoan2(name: String): Maybe[Money] =
    Api.getAccount(name)
      .bind(x => Api.getBalance(x)
        .bind(Api.qualifiedAmount))

  def getLoan3(name: String): Maybe[Money] =
    Api.getAccount(name)
      .bind(Api.getBalance)
      .bind(Api.qualifiedAmount)

  def run(): Unit = {
    for (name <- List("Alex", "Andrew", "Nick", "Iren", "Fake")) {
      println(s"$name -> ${getLoan1(name)}, ${getLoan2(name)}, ${getLoan3(name)}")
    }
  }

  def main(args: Array[String]): Unit = {
    println(getClass.getSimpleName)
    run()
    println()
  }
}

object MaybeAKAOption4 {
  sealed trait Maybe[+A] {
    def flatMap[B](f: A => Maybe[B]): Maybe[B] =
      this match {
        case Just(x) => f(x)
        case Nothing => Nothing
      }

    def map[B](f: A => B): Maybe[B] =
      this match {
        case Just(x) => Just(f(x))
        case Nothing => Nothing
      }
  }
  final case class Just[A](value: A) extends Maybe[A]
  case object Nothing extends Maybe[Nothing]

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Maybe[Account] = {
      name match {
        case "Andrew" => Just(Account(1))
        case "Alex" => Just(Account(2))
        case "Nick" => Just(Account(3))
        case "Iren" => Just(Account(4))
        case _ => Nothing
      }
    }

    def getBalance(account: Account): Maybe[Money] = {
      account.id match {
        case 1 => Just(1000)
        case 2 => Just(100)
        case _ => Nothing
      }
    }

    def qualifiedAmount(amount: Money): Maybe[Money] = {
      if (amount >= 1000) Just(1000)
      else Nothing
    }
  }

  def getLoan(name: String): Maybe[Money] =
    for {
      account <- Api.getAccount(name)
      balance <- Api.getBalance(account)
      loan <- Api.qualifiedAmount(balance)
    } yield loan

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

object MaybeAKAOption5 {
  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Option[Account] = {
      name match {
        case "Andrew" => Some(Account(1))
        case "Alex" => Some(Account(2))
        case "Nick" => Some(Account(3))
        case "Iren" => Some(Account(4))
        case _ => None
      }
    }

    def getBalance(account: Account): Option[Money] = {
      account.id match {
        case 1 => Some(1000)
        case 2 => Some(100)
        case _ => None
      }
    }

    def qualifiedAmount(amount: Money): Option[Money] = {
      if (amount >= 1000) Some(1000)
      else None
    }
  }

  def getLoan(name: String): Option[Money] =
    for {
      account <- Api.getAccount(name)
      balance <- Api.getBalance(account)
      loan <- Api.qualifiedAmount(balance)
    } yield loan

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