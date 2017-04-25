package recfun


object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    c match {
      case 0 => 1
      case c if r < c => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def process(chars: List[Char], myStack: List[Char]): Boolean =
      if (chars.isEmpty) myStack.isEmpty
      else {
        chars.head match {
          case '(' => process(chars.tail, myStack.::(chars.head))
          case ')' => if (myStack.contains('(')) process(chars.tail, myStack.tail)
          else false
          case _ => process(chars.tail, myStack)
        }
      }

    val balancingAuxStack = List[Char]()

    process(chars, balancingAuxStack)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    money match {
      case 0 => 1
      case x if x < 0 => 0
      case x if x >= 1 && coins.isEmpty => 0
      case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
