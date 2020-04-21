package scas.power

import scas.math.Numeric
import scas.structure.ordered.Monoid
import scas.variable.Variable

abstract class PowerProduct[M: ClassTag] extends Monoid[M] {
  def variables: Seq[Variable]
  val length = variables.length
  def generator(variable: String): M = generator(variables.indexOf(variable))
  def generator(n: Int): M
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def degree(x: M): Long
  def apply(x: Int) = {
    assert (x == 1)
    one
  }
  def gcd(x: M, y: M): M
  def lcm(x: M, y: M): M
  def coprime(x: M, y: M) = gcd(x, y).isOne
  def (x: M) / (y: M): M
  def (x: M) | (y: M): Boolean
  def (x: M).isUnit = x.isOne
  def dependencyOnVariables(x: M): Array[Int]
  def (x: M).projection(n: Int): M
  override def toString = "[" + variables.mkString(", ") + "]"
  def toMathML = s"<list>${variables.map(a => a.toMathML)}</list>"
  override def apply(x: M) = x.convert(variables: _*)
  def (x: M).convert(from: Variable*): M
  def size(x: M): Int
}

object PowerProduct {
  def apply[M : PowerProduct] = summon[PowerProduct[M]]
}
