package scas.power

import scas.structure.ordered.Monoid
import scas.variable.Variable
import scas.base.BigInteger

trait PowerProduct[M] extends Monoid[M] {
  def variables: Seq[Variable]
  val length = variables.length
  def generator(variable: String): M = generator(variables.indexOf(variable))
  def generator(n: Int): M
  def generators = (for (i <- 0 until length) yield generator(i)).toList
  def degree(x: M): BigInteger
  def apply(x: Int) = {
    assert (x == 1)
    one
  }

  def gcd(x: M, y: M): M
  def lcm(x: M, y: M): M
  def coprime(x: M, y: M) = gcd(x, y).isOne
  extension (x: M) {
    def divide(y: M): M
    def factorOf(y: M): Boolean
    inline def / (y: M) = x.divide(y)
    inline def | (y: M) = x.factorOf(y)
    def isUnit = x.isOne
  }
  def dependencyOnVariables(x: M): Array[Int]
  extension (x: M) def projection(n: Int): M
  override def toString = variables.toList.show
  def toMathML = variables.toList.toMathML
  extension (x: M) def convert(from: PowerProduct[M]): M
  def size(x: M): Int

  given int2powerProduct: (Int => M) = this(_)
}
