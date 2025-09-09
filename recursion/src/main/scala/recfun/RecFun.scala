package recfun

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
   */
  def pascal(c: Int, r: Int): Int = {
    def loop(level: Int): Vector[Int] = 
      if (level == 0) Vector(1)
      else if (level == 1) Vector(1, 1)
      else {
        val prev = loop(level - 1)

        val middle = for (i <- 1 until prev.length) yield prev(i - 1) + prev(i)

        (1 +: middle :+ 1).toVector 
      }

      loop(r)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(open: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) open == 0
      else {
        if (chars.head == '(') loop(open + 1, chars.tail)
        else if (chars.head == ')') loop(open - 1, chars.tail)
        else loop(open, chars.tail)
      }

    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
