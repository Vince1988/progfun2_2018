package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (name, signal) => (name, Signal(eval(name, signal(), namedExpressions)))
    }
  }

  def eval(name: String, expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(n) if name != n => eval(name, getReferenceExpr(n, references), references)
    case Plus(a, b) => eval(name, a, references) + eval(name, b, references)
    case Minus(a, b) => eval(name, a, references) - eval(name, b, references)
    case Times(a, b) => eval(name, a, references) * eval(name, b, references)
    case Divide(a, b) => eval(name, a, references) / eval(name, b, references)
    case _ => Double.NaN
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr](Literal(Double.NaN))(exprSignal => exprSignal())
  }
}
