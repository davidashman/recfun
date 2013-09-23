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

  def pascal(c: Int, r: Int): Int = {
    // first column, first row and last column are always 1
    if (c == 0 || r == 0 || c == r)
      return 1;

    // otherwise, look at the two above it
    return pascal(c - 1, r - 1) + pascal(c, r - 1);
  }

  def balance(chars: List[Char]): Boolean = {
    return balance(chars, 0)
  }

  def balance(chars: List[Char], openCount: Int): Boolean = {
    // if we've drained the characters and the openCount == 0, we're good
    if (chars.isEmpty)
      return (openCount == 0)

    // if we see an open paren, increment the counter
    if (chars.head == '(')
      return balance(chars.tail, openCount + 1)

    // if we see a close paren, decrement, unless we're already at zero, which means
    // we have an unbalanced close
    if (chars.head == ')')
      if (openCount > 0)
        return balance(chars.tail, openCount - 1)
      else
        return false

    // not a paren, so move on, nothing to see here
    return balance(chars.tail, openCount)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    // Zero amount, ony one way to count it
    if (money == 0)
      return 1

    // Less than zero amount, no way to count that
    if (money < 0)
      return 0

    // No more coins, no way to count than
    if (coins.length == 0)
      return 0

    // The total permutations equals the permutations for that amount with one less coin option
    // plus the permutations for the amount minus one of the coins and all of the coins
    return countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
  
}
