package scas.power

import scas.math.Numeric
import scas.structure.ordered.Monoid
import scas.variable.Variable
import scas.prettyprint.Show

abstract class PowerProduct[M: ClassTag] extends Monoid[M] {
  given PowerProduct[M] = this
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
  extension[U] (x: U)(using c: U => M) {
    def / (y: M) = c(x).divide(y)
    def | (y: M) = c(x).factorOf(y)
  }
  extension (x: M) {
    def /[U](y: U)(using c: U => M) = x.divide(c(y))
    def |[U](y: U)(using c: U => M) = x.factorOf(c(y))
    def divide(y: M): M
    def factorOf(y: M): Boolean
    def isUnit = x.isOne
  }
  def dependencyOnVariables(x: M): Array[Int]
  extension (x: M) def projection(n: Int): M
  override def toString = Show.listed(variables.map(_.toString): _*)
  def toMathML = Show.list(variables.map(_.toMathML): _*)
  override def apply(x: M) = x.convert(variables: _*)
  extension (x: M) def convert(from: Variable*): M
  def size(x: M): Int
}

object PowerProduct {
  def apply[M : PowerProduct] = summon[PowerProduct[M]]
}
