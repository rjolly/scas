package scas.power

import scas.util.{Conversion, unary_~}
import scas.structure.ordered.Monoid
import scas.variable.Variable
import scas.base.BigInteger
import scas.prettyprint.Show.given

trait PowerProduct[M] extends Monoid[M] {
  def variables: Seq[Variable]
  def length = variables.length
  def generator(variable: Variable): M = generator(variables.indexOf(variable))
  def generator(n: Int): M
  def generators = (for i <- 0 until length yield generator(i)).toList
  def apply(x: Int) = {
    assert (x == 1)
    one
  }
  def gcd(x: M, y: M): M
  def lcm(x: M, y: M): M
  def coprime(x: M, y: M) = gcd(x, y).isOne
  extension (x: M) {
    def degree: BigInteger
    def divide(y: M): M
    def factorOf(y: M): Boolean
    inline def / [U: Conversion[M]](y: U) = x.divide(~y)
    inline def | [U: Conversion[M]](y: U) = x.factorOf(~y)
    def isUnit = x.isOne
    def dependencyOnVariables: Array[Int]
    def projection(n: Int): M = projection(n, n + 1)
    def projection(n: Int, m: Int): M
    def convert(from: PowerProduct[M]): M
    override def convert: M = x.convert(this)
    def size: Int
    def toCode(level: Level) = toCode(level, "*")
    def toCode(level: Level, times: String): String
    def toMathML = toMathML("times")
    def toMathML(times: String): String
  }
  override def toString = variables.toList.show
  def toMathML = variables.toList.toMathML

  given int2powerProduct: (Int => M) = this(_)
}

object PowerProduct {
  trait Conv[M] extends PowerProduct[M] with Monoid.Conv[M] {
    extension[U: Conversion[M]] (x: U) {
      inline def / (y: M) = (~x).divide(y)
      inline def | (y: M) = (~x).factorOf(y)
    }
  }
}
