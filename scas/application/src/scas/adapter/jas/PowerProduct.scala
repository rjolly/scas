package scas.adapter.jas

import scas.variable.Variable
import edu.jas.poly.{ExpVector, TermOrder}
import scas.util.{Conversion, unary_~}
import scas.base.BigInteger
import BigInteger.given

class PowerProduct(val variables: Variable*)(tord: TermOrder) extends scas.power.PowerProduct[ExpVector] {
  val comp = tord.getDescendComparator
  val one = ExpVector.create(length)
  def generator(n: Int) = ExpVector.create(length, n, 1)
  extension (x: ExpVector) def degree = BigInteger.fromInt(x.degree)
  def gcd(x: ExpVector, y: ExpVector) = x.gcd(y)
  def lcm(x: ExpVector, y: ExpVector) = x.lcm(y)
  extension (x: ExpVector) {
    def multiply(y: ExpVector) = x.sum(y)
    def divide(y: ExpVector) = x.subtract(y)
    def factorOf(y: ExpVector) = x.divides(y)
  }
  def compare(x: ExpVector, y: ExpVector) = comp.compare(x, y)
  extension (x: ExpVector) {
    def dependencyOnVariables = x.dependencyOnVariables
    def projection(n: Int, m: Int) = ???
    def convert(from: scas.power.PowerProduct[ExpVector]) = ???
    def size = x.dependentVariables
    def toCode(level: Level, times: String) = x.toString(variables.map(_.toString).toArray)
    def toMathML(times: String) = ???
  }
}

object PowerProduct {
  def apply[U: Conversion[Variable]](variables: U*)(tord: TermOrder) = new Conv(variables.map(~_)*)(tord)

  class Conv(variables: Variable*)(tord: TermOrder) extends PowerProduct(variables*)(tord) with scas.power.PowerProduct.Conv[ExpVector] {
    given instance: Conv = this
  }
}
