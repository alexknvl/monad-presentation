object NonDeterminism1 {
  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): List[Account] = {
      name match {
        case "Andrew" => List(Account(1), Account(5))
        case "Alex" => List(Account(2))
        case "Nick" => List(Account(3))
        case "Iren" => List(Account(4))
        case _ => Nil
      }
    }

    def getBalance(account: Account): List[Money] = {
      account.id match {
        case 1 => List(1000)
        case 2 => List(100, 2000)
        case 5 => List(10000)
        case _ => Nil
      }
    }

    def qualifiedAmount(amount: Money): List[Money] = {
      if (amount >= 1000) List(1000)
      else Nil
    }
  }

  def getLoan(name: String): List[Money] = {
    val arr = List.newBuilder[Money]

    for (account <- Api.getAccount(name)) {
      for (balance <- Api.getBalance(account)) {
        for (loan <- Api.qualifiedAmount(balance)) {
          arr += loan
        }
      }
    }

    arr.result()
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

object NonDeterminism2 {
  case class Account(id: Int)
  type Money = BigInt

  object Api {
    def getAccount(name: String): List[Account] = {
      name match {
        case "Andrew" => List(Account(1), Account(5))
        case "Alex" => List(Account(2))
        case "Nick" => List(Account(3))
        case "Iren" => List(Account(4))
        case _ => Nil
      }
    }

    def getBalance(account: Account): List[Money] = {
      account.id match {
        case 1 => List(1000)
        case 2 => List(100, 2000)
        case 5 => List(10000)
        case _ => Nil
      }
    }

    def qualifiedAmount(amount: Money): List[Money] = {
      if (amount >= 1000) List(1000)
      else Nil
    }
  }

  implicit class ListWrapper[A](val list: List[A]) {
    def flatMap1[B](f: A => List[B]): List[B] = {
      val builder = List.newBuilder[B]
      for (a <- list) {
        builder ++= f(a)
      }
      builder.result()
    }
  }

  def getLoanOld(name: String): List[Money] = {
    val arr = List.newBuilder[Money]

    for (account <- Api.getAccount(name)) {
      for (balance <- Api.getBalance(account)) {
        for (loan <- Api.qualifiedAmount(balance)) {
          arr += loan
        }
      }
    }

    arr.result()
  }

  def getLoan(name: String): List[Money] =
    Api.getAccount(name)
      .flatMap1(Api.getBalance)
      .flatMap1(Api.qualifiedAmount)

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