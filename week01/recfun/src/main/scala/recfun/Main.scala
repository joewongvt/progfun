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
    recursiveCountChange(money, unique(coins))
  }

  def unique(l: List[Int]): List[Int] = {
    removeDuplicatesAndIllegalValues(l, List())
  }

  def removeDuplicatesAndIllegalValues(l: List[Int], dedup: List[Int]): List[Int] = {
    if (l.isEmpty) dedup
    else if (l.head > 0 && !listContains(dedup, l.head)) removeDuplicatesAndIllegalValues(l.tail, dedup :+ l.head)
    else removeDuplicatesAndIllegalValues(l.tail, dedup)
  }

  def listContains(l: List[Int], v: Int): Boolean = {
    if (l.isEmpty) false
    else if (l.head == v) true
    else listContains(l.tail, v)
  }


  def recursiveCountChange(a: Int, n: List[Int]): Int = {
    // assumes n is deduplicated and negative values are removed

    /* is this integration by parts?
    change a with n kinds of coins, from SICP

    If a is exactly 0, we should count that as 1 way to make change.
    If a is less than 0, we should count that as 0 ways to make change.
    If n is 0, we should count that as 0 ways to make change.
    */

    if (a == 0) 1
    else if (a < 0) 0
    else if (n.isEmpty) 0
    else recursiveCountChange(a, n.tail) + recursiveCountChange(a-n.head, n)

    // a = 100; n = [50, 25]
    // rcc(100, 25)                                                                    + rcc(50,[50,25])
    // rcc(100, empty) + rcc(75, 25)                                                   + rcc(50, 25)                                  + rcc(0, [50, 25])
    // 0               + rcc(75, empty) + rcc(50, 25)                                  + rcc(50, empty) + rcc(25, 25)                 + 1
    // 0               + 0              + rcc(50, empty) + rcc(25,25)                  + 0              + rcc(25, empty) + rcc(0, 25) + 1
    // 0               + 0              + 0              + rcc(25, empty) + rcc(0, 25) + 0              + 0              + 1          + 1
    // 0               + 0              + 0              + 0              + 1          + 0              + 0              + 1          + 1
    // 3

  }

}
