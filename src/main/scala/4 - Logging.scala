object Logging1 {
  case class Log[A](value: A, output: Vector[String])
  object Log {
    def apply[A](value: A): Log[A] = new Log(value, Vector.empty)
    def apply[A](value: A, message: String): Log[A] = new Log(value, Vector(message))
    def log(string: String): Log[Unit] = new Log((), Vector(string))
  }

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Log[Account] = {
      name match {
        case "Andrew" => Log(Account(1), s"Found account with a name $name.")
        case "Alex" => Log(Account(2), s"Found account with a name $name.")
        case "Nick" => Log(Account(3), s"Found account with a name $name.")
        case "Iren" => Log(Account(4), s"Found account with a name $name.")
        case _ => Log(Account(-1), s"Unknown account name $name.")
      }
    }

    def getBalance(account: Account): Log[Money] = {
      account.id match {
        case 1 => Log(1000, s"Found a balance sheet for account ${account.id}.")
        case 2 => Log(100, s"Found a balance sheet for account ${account.id}.")
        case 5 => Log(10000, s"Found a balance sheet for account ${account.id}.")
        case _ => Log(0, s"No balance associated with account ${account.id}.")
      }
    }

    def qualifiedAmount(amount: Money): Log[Money] = {
      if (amount >= 1000) Log(1000, s"Funds ($amount) are sufficient for a loan.")
      else Log(0, s"Insufficient funds for a loan, current balance is $amount.")
    }
  }

  def getLoan(name: String): Log[Money] = {
    val Log(account, log1) = Api.getAccount(name)
    val Log(balance, log2) = Api.getBalance(account)
    val Log(loan, log3) = Api.qualifiedAmount(balance)
    Log(loan, log1 ++ log2 ++ log3)
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

object Logging2 {
  case class Log[A](value: A, output: Vector[String]) {
    def flatMap[B](f: A => Log[B]): Log[B] = {
      val result = f(this.value)
      result.copy(output = this.output ++ result.output)
    }

    def map[B](f: A => B): Log[B] =
      new Log(f(value), output)
  }
  object Log {
    def apply[A](value: A): Log[A] = new Log(value, Vector.empty)
    def apply[A](value: A, message: String): Log[A] = new Log(value, Vector(message))
    def log(string: String): Log[Unit] = new Log((), Vector(string))
  }

  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): Log[Account] = {
      name match {
        case "Andrew" => Log(Account(1), s"Found account with a name $name.")
        case "Alex" => Log(Account(2), s"Found account with a name $name.")
        case "Nick" => Log(Account(3), s"Found account with a name $name.")
        case "Iren" => Log(Account(4), s"Found account with a name $name.")
        case _ => Log(Account(-1), s"Unknown account name $name.")
      }
    }

    def getBalance(account: Account): Log[Money] = {
      account.id match {
        case 1 => Log(1000, s"Found a balance sheet for account ${account.id}.")
        case 2 => Log(100, s"Found a balance sheet for account ${account.id}.")
        case 5 => Log(10000, s"Found a balance sheet for account ${account.id}.")
        case _ => Log(0, s"No balance associated with account ${account.id}.")
      }
    }

    def qualifiedAmount(amount: Money): Log[Money] = {
      if (amount >= 1000) Log(1000, s"Funds ($amount) are sufficient for a loan.")
      else Log(0, s"Insufficient funds for a loan, current balance is $amount.")
    }
  }

  def getLoan(name: String): Log[Money] = for {
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