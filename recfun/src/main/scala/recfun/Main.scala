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
      // all edges (left & right) are 1 and all (leftmost + 1 & rightmost - 1) position contains row number
      if (c == 0 || c == r) 1 else if (c == 1 || c == r - 1) r else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def doBalance(bal: Int, cs: List[Char]): Boolean = {
        if (cs.isEmpty) bal == 0                                        // simple check
        else if (cs.head == '(') doBalance(bal + 1, cs.tail)            // when open brace we can increment
        else if (cs.head == ')') bal > 0 && doBalance(bal - 1, cs.tail) // this time, we can close if we already open it
        else doBalance(bal, cs.tail)
      }
      doBalance(0, chars)
    }
  
  /**
   * Exercise 3
   *
   * Main concept is from https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.2
   * (Example: Counting change)
   * Core concept:
   * count change = the number of ways to change amount a using all but the first kind of coin +
   *               the number of ways to change amount a - d using all n kinds of coins, where d is the denomination
   *               of the first kind of coin.
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
