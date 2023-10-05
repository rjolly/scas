package jas

import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import edu.jas.poly.{ExpVector, TermOrder}
import scas.base.BigInteger
import BigInteger.given

class PowerProduct(val variables: Variable*)(tord: TermOrder) extends scas.power.PowerProduct[ExpVector] {
  given instance: PowerProduct = this
  val comp = tord.getDescendComparator
  def one = ExpVector.create(length)
  def generator(n: Int) = ExpVector.create(length, n, 1)
  def degree(x: ExpVector) = BigInteger.fromInt(x.degree)
  def gcd(x: ExpVector, y: ExpVector) = x.gcd(y)
  def lcm(x: ExpVector, y: ExpVector) = x.lcm(y)
  extension (x: ExpVector) {
    def multiply(y: ExpVector) = x.sum(y)
    def divide(y: ExpVector) = x.subtract(y)
    def factorOf(y: ExpVector) = x.divides(y)
  }
  def compare(x: ExpVector, y: ExpVector) = comp.compare(x, y)
  def dependencyOnVariables(x: ExpVector) = x.dependencyOnVariables
  extension (x: ExpVector) {
    def projection(n: Int) = ???
    def toCode(level: Level) = x.toString(variables.map(_.toString).toArray)
    def toMathML = ???
    def convert(from: scas.power.impl.PowerProduct[ExpVector]) = ???
  }
  def size(x: ExpVector) = x.dependentVariables
}

object PowerProduct {
  def apply[U: Conversion[Variable]](variables: U*)(tord: TermOrder) = new PowerProduct(variables.map(~_): _*)(tord)
}
