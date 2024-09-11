package recfun
import common._
import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int =
    if (c==0||c==r) 1 else pascal(c, r-1) + pascal(c-1, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceP (chars: List[Char], pcnt: Int): Boolean = {
      if (chars.isEmpty) pcnt==0
      else if (pcnt<0) false
      else {
        if (chars.head=='(')
          balanceP(chars.tail, pcnt+1)
        else if (chars.head==')')
          balanceP(chars.tail, pcnt-1)
        else balanceP(chars.tail, pcnt)
      }
    }
    balanceP(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money==0) 1
    else if (coins.isEmpty||money<0) 0
    else {
      countChange(money-coins.head, coins)+countChange(money,coins.tail)
    }
  }
}
