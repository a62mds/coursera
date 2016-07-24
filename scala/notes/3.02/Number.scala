package Number

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
