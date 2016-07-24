package decomp

trait Expr {
  def eval: Int = this match {
    case Var(x)      => throw new Error(s"Cannot evaluate variable $x")
    case Number(n)   => n
    case Sum (e1,e2) => e1.eval + e2.eval
    case Diff(e1,e2) => e1.eval - e2.eval
    case Prod(e1,e2) => e1.eval * e2.eval
    case Div (e1,e2) => e1.eval / e2.eval
  }
  def show: String = this match {
    case Var(x)      => x
    case Number(n)   => n.toString
    case Sum (e1,e2) => e1.show + " + " + e2.show
    case Diff(e1,e2) => e1.show + " - " + e2.show
    case Prod(e1,e2) => e1.show + " * " + e2.show
    case Div (e1,e2) => e1.show + " / " + e2.show
  }
}
case class Var(x: String) extends Expr {}
case class Number(n: Int) extends Expr {}
case class Sum (e1: Expr, e2: Expr) extends Expr {}
case class Diff(e1: Expr, e2: Expr) extends Expr {}
case class Prod(e1: Expr, e2: Expr) extends Expr {}
case class Div (e1: Expr, e2: Expr) extends Expr {}
