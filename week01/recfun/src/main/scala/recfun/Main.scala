package recfun
import common._

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
    // n == row, k == col
    // The binomial coefficient \scriptstyle {n \choose k} is conventionally set to zero if k is either less than zero or greater than n
    if (r < 0 || c < 0 || c > r) 0
    else if (c == 0 || c==r) 1
    else pascal(c-1, r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balancePairs(str: List[Char], openCount: Int): Boolean = {
      println(str.mkString + ": openCount=" + openCount)

      if (str.isEmpty) openCount == 0
      else if (openCount == 0 && str.head == ')') false
      else {
        def headScore = if (str.head == '(') 1 else if (str.head == ')') -1 else 0
        balancePairs(str.tail, openCount + headScore)
      }
    }

    if (chars.isEmpty) true
    else balancePairs(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money < 1) 1
    else {
      val validDenominations = coins.filter((c:Int) => c <= money)
      if (coins.isEmpty || validDenominations.isEmpty) 0
      else -1 //FIXME
    }
  }
}
