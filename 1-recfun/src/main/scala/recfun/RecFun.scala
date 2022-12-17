package recfun

import scala.annotation.tailrec

// 10 / 10

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   * 0,0 is the top of the pyramid
   * c = col, 0-indexed
   * r = row, 0-indexed
   */
  def pascal(c: Int, r: Int): Int = {
    //at the pyramid edges
    if (c == 0 || r == 0 || c >= r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(charsA: List[Char]): Boolean = {
    @tailrec
    def balanceHelper(chars: List[Char], leftCount: Int): Boolean = {
      if (chars.isEmpty)
        leftCount == 0
      else
        chars.head match {
          case '(' => balanceHelper(chars.tail, leftCount + 1)
          case ')' if leftCount == 0 => false
          case ')' => balanceHelper(chars.tail, leftCount - 1)
          case _ => balanceHelper(chars.tail, leftCount)
        }
    }

    balanceHelper(charsA, 0)
  }

  /**
   * Exercise 3
   * assumed coins are unique
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0 || coins.isEmpty)
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
