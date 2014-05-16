/**
 * Created by joshuaw on 5/15/14.
 */
class Rational(x: Int, y: Int) {
//  require(y > 0, "Denominator must be positive")

  def this(x: Int) = this(x,1)

  def numer = x / g
  def denom = y / g

  def +(that: Rational): Rational = new Rational(numer*that.denom + that.numer*denom, denom*that.denom)
  def unary_- : Rational = new Rational(-x, y)
  def -(that: Rational): Rational = this + -that
  def < (that: Rational): Boolean = numer * that.denom < that.numer * denom
  def max(that: Rational): Rational = if(this < that) that else this


  override def toString = numer + "/" + denom

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x,y)
}
