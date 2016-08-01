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
      if (c == 0 || r == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def inc(x: Int) = {
        if (x >= 0) x + 1
        else x
      }

      def unclosed(open: Int, chars: List[Char]) : Int = {
        if (chars.isEmpty) open
        else {
          chars.head match {
            case '(' => unclosed(inc(open), chars.tail)
            case ')' => unclosed(open - 1, chars.tail)
            case _ => unclosed(open, chars.tail)
          }
        }
      }
      unclosed(0, chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def changeWithMax(value: Int, max: Int): Int = {
        countChange(value, coins.filter((v: Int) => v <= max))
      }

      def tryCoin(unChecked: List[Int]): Int = {
        if (unChecked.tail.isEmpty) changeWithMax(money - unChecked.head, unChecked.head)
        else changeWithMax(money - unChecked.head, unChecked.head) + tryCoin(unChecked.tail)
      }

      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else tryCoin(coins)
    }
  }
