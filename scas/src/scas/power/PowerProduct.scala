package scas.power

import scas.util.{Conversion, unary_~}
import scas.structure.ordered.Monoid
import scas.variable.Variable
import scas.base.BigInteger

abstract class PowerProduct[M] extends Monoid[M] {
  def variables: Seq[Variable]
  def length = variables.length
  def take(n: Int) = newInstance(variables.take(n)*)
  def drop(n: Int) = newInstance(variables.drop(n)*)
  def newInstance(variables: Variable*): PowerProduct[M]
  def generator(variable: Variable): M = generator(variables.indexOf(variable))
  def generator(n: Int): M
  def generators = (for (i <- 0 until length) yield generator(i)).toList
  extension (x: M) def degree: BigInteger
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
    inline def / [U: Conversion[M]](y: U) = x.divide(~y)
    inline def | [U: Conversion[M]](y: U) = x.factorOf(~y)
    def isUnit = x.isOne
  }
  def dependencyOnVariables(x: M): Array[Int]
  extension (x: M) {
    def projection(n: Int): M = projection(n, n + 1)
    def projection(n: Int, m: Int): M
    def convert(from: PowerProduct[M]): M
  }
  override def toString = variables.toList.show
  def toMathML = variables.toList.toMathML
  extension (x: M) def size: Int

  given int2powerProduct: (Int => M) = this(_)
}
