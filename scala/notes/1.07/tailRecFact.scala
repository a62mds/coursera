object tailRecFact extends App {
	
  def fact(n: Int): Int = {
    def factIter(a: Int, acc: Int): Int = 
      if (a == 0) acc
      else factIter(a - 1, a * acc)
    factIter(n, 1)
  }

  val n = 5
  println(s"$n! = " + fact(n))
}
