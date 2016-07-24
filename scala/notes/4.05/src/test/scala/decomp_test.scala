package decomp

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExprTest extends FunSuite {
  
  test("Expr.eval returns n when Expr is Number(n)") {
    val aNum = new Number(1)
    assert(aNum.eval==1)
  }

  test("Expr.show returns 'n' when Expr is Number(n)") {
    val aNum = new Number(10)
    assert(aNum.show=="10")
  }

  test("Expr.eval returns n+m when Expr is Sum(Number(n),Number(m))") {
    val aSum = new Sum(new Number(5), new Number(4))
    assert(aSum.eval==5+4)
  }
  
  test("Expr.show returns 'n+m' when Expr is Sum(Number(n),Number(m))") {
    val aSum = new Sum(new Number(1), new Number(2))
    assert(aSum.show=="1 + 2")
  }

  test("Expr.eval returns n-m when Expr is Diff(Number(n),Number(m))") {
    val aDif = new Diff(new Number(5), new Number(3))
    assert(aDif.eval==5-3)
  }

  test("Expr.show returns 'n-m' when Expr is Diff(Number(n),Number(m))") {
    val aDif = new Diff(new Number(2), new Number(7))
    assert(aDif.show=="2 - 7")
  }

  test("Expr.eval returns n*m when Expr is Prod(Number(n),Number(m))") {
    val aProd = new Prod(new Number(934), new Number(34))
    assert(aProd.eval==934*34)
  }

  test("Expr.show returns 'n*m' when Expr is Prod(Number(n),Number(m))") {
    val aProd = new Prod(new Number(1), new Number(3))
    assert(aProd.show=="1 * 3")
  }

}
