package spire.macros

import scala.reflect.macros.Context

object Ops {
  def unop[R](c:Context)():c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs)))
  }
  def binop[A, R](c:Context)(rhs:c.Expr[A]):c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs, rhs.tree)))
  }
  def unpack[T[_], A](c:Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(x)), List(ev)) => (ev, x)
      case Apply(Select(ev, _), List(x)) => (ev, x)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operator (tree = %s)" format t)
    }
  }
  private final val operatorNames = Map(
    // Equiv (>< <>)
    "$greater$less" -> "equiv",
    "$less$greater" -> "nequiv",

    // Ordering (> >= < <=)
    "$greater" -> "gt",
    "$greater$eq" -> "gteq",
    "$less" -> "lt",
    "$less$eq" -> "lteq",

    // AbelianGroup (unary_+ unary_- + -)
    "unary_$plus" -> "identity",
    "unary_$minus" -> "negate",
    "$plus" -> "plus",
    "$minus" -> "minus",

    // SemiGroup (*)
    "$times" -> "times",

    // UFD (/ % /% |)
    "$div" -> "divide",
    "$percent" -> "remainder",
    "$div$percent" -> "divideAndRemainder",
    "$bar" -> "factorOf"
  )
  def findMethodName(c:Context) = {
    import c.universe._
    val s = c.macroApplication.symbol.name.toString
    TermName(operatorNames.getOrElse(s, s))
  }
}
