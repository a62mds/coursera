object Sqrt extends App {

  val tolerance = 0.0000001

  def abs(x: Double): Double = if (x<0) -x else x

  def square(x: Double): Double = x * x

  def d(x: Double, y: Double): Double = abs(x - y)

  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    def isGoodEnough(guess: Double): Boolean =
      d(guess, x/guess) < tolerance
    def improve(guess: Double): Double =
      (guess + x / guess) / 2
    sqrtIter(1.0)
  }
  def printSqrt(x: Double) = println(s"sqrt($x) = "+sqrt(x))

  val x: Double = 2
  val eps: Double = 0.1e-6
  val omg: Double = 1.0e20
  val alp: Double = 1.0e50
  printSqrt(x)
  printSqrt(eps)
  printSqrt(omg)
  printSqrt(alp)
}
