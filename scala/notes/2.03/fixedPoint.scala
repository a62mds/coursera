
object fixedPoint extends App {

  val tolerance = 0.0001

  println(fixedPoint(x => 1 + 0.5*x)(1))

  val num = 2
  println(s"sqrt($num) = " + sqrt(num) + s" +- $tolerance")

  def isCloseEnough(x: Double, y: Double): Boolean = {
    abs((x - y) / x) / x < tolerance
  }

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      //println(" guess = " + guess)
      val next = f(guess)
      if (isCloseEnough(guess,next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def abs(x: Double): Double = {
    if (x >= 0.0) x
    else -x
  }

  //def sqrt(x: Double): Double = fixedPoint(y => (0.5*(y + x/y)))(1.0)

  def averageDamp(f: Double => Double)(x: Double): Double = 0.5*(x + f(x))

  def g(x: Double, y: Double): Double = x/y
  def sqrt(x: Double): Double = fixedPoint(averageDamp(g(x,_)))(1.0)
}
