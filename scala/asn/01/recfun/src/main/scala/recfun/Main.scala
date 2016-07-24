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
    // If an edge element is requested, return 1.
    if (r == 0 || r == c || c == 0) 1
    // Otherwise, use the definition to calculate the element. This will
    // recursively call itself until the edges of the triangle are reached. The
    // results then descent the stack as it collapses (not a tail recursive
    // algorithm)
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def walkList(chars: List[Char], count: Int): Boolean = {
      //println(s"walkList called with count = $count")
      // This branch of the if statment gains control when the entire list has
      // been checked, and so it will be the statement that determines the
      // return value of balance. 
      // If the number of opening parentheses equals the number of closed
      // parentheses, return true; otherwise return false.
      if (chars.isEmpty) count == 0
      // If the first character in the list is an opening parenthesis, increase
      // the count and keep walking the list by calling walkList on the tail of
      // chars. At any point in the recursion chain, the value of count gives
      // the number of opened parentheses that need closing.
      else if (chars.head == '(') { 
        //println(chars.head + s"  :  count = $count")
        walkList(chars.tail, count + 1)
      }
      // If there is at least one open parenthesis that needs closing, and if
      // the first character in the list is a closing parenthesis, decrease
      // the count and keep walking the list by calling walkList on the tail of
      // chars.
      else if (chars.head == ')') { 
        //println(chars.head + s"  :  count = $count")
        count > 0 && walkList(chars.tail, count - 1)
      }
      // If the first character in the list is not a parenthesis, keep the
      // count the same and keep walking the list
      else walkList(chars.tail, count)
    }
    walkList(chars,0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      // Traverse each branch of the tree of possible combinations of the coins
      // seeking those branches whose total value is equal to money. A sort of 
      // running total is kept along each branch of the tree by recursively 
      // calling countChange with money reduced by the denomination currently 
      // at the head of coins. Each branch ends after a finite number of steps 
      // at which point a value of either 1 or 0 is assigned to the branch, 
      // depending on the value of the running total at the end of the branch.
      //
      // There are three criteria for ending a branch:
      //      1) If the value of (money - running_total) < 0, the branch ends
      //         with a value of 0, since the running total has exceeded money
      //      2) If the value of (money - running_total) = 0, the branch ends
      //         with a value of 1, since the running total is equal to money
      //      3) If the value of (money - running_total) > 0, a branch is
      //         ended if there are no remaining coins to choose from.
      //
      // Understanding of these criteria benefits from further discussion.
      // What's being sought here is the number of ordered n-tuples
      //                  (alpha_1, ..., alpha_n) in N0^n
      // where N0 = naturals u {0} = {0,1,2,...} and n = #(coins), that satisfy
      //        alpha_1 * coins[1] + ... + alpha_n * coins[n] = money. 
      // Criterion 1) can be thought of as the rule for excluding those tuples
      // which result in a sum that is greater than money, while criterion 3) 
      // can be thought of as the rule for excluding those tuples which result 
      // in a sum that is less than money. Criterion 2), on the other hand,
      // is the rule for including---i.e. counting---those tuples whose sum is
      // exactly money: exactly what is being sought.
      if (money == 0) 1
      // If the total along the branch is still less than money and the branch
      // hasn't reached its end, fork the branch and explore each new branch
      // for possible combinations whose total is money
      else if (money > 0 && !coins.isEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      // If control reaches this point, the running total has exceeded money,
      // or the total of this branch has fallen short of money, so it is not 
      // counted
      else 0
      // If any people are reading this, I do not claim authorship of the code
      // for this question. The understanding of the code and how it works I do
      // take credit for the above explanation.
    }
}
