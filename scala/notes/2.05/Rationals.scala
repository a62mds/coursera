


object Rationals extends App {
 /* 
  val p = new Rational(1, 2)
  val q = new Rational(3, 4)

  println(p.add(q).toString)
  println(q.neg.toString)
  println(q.sub(p).toString)

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  println(x.sub(y).sub(z))
  println(x.less(y))
  println(!x.more(y))
  println(x.max(y))

  //val div0 = new Rational(1, 0)
*/

 val p = new Rational(1,3)
 val q = new Rational(1,4)
 println(p.toString + " + " + q.toString + " = " + (p + q).toString)

}

class Rational(x:Int, y:Int) {
  require(y != 0, "Denominator cannot be 0")

  // alternate constructor
  def this(x: Int) = this(x, 1)
  // if "this" is used as the name of a function in a class, that function is
  // interpreted as a constructor

  private def gcd(a:Int, b:Int):Int = if (b==0) a else gcd(b, a % b)
  val g = gcd(x, y)
  def numer = x/g
  def denom = y/g

  override def toString = numer + "/" + denom

  def +(that: Rational) = {
    new Rational(this.numer*that.denom + that.numer*this.denom, this.denom*that.denom)
  }

  def unary_- : Rational = new Rational(-numer, denom)

  def -(that: Rational): Rational = this + -that

  def <(that: Rational) = this.numer * that.denom < that.numer * this.denom
  def >(that: Rational) = !(this < that)

  def max(that: Rational) = if (this < that) that else this
  def min(that: Rational) = if (this < that) this else that
}
