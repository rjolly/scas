package jas.conversion

import scas.variable.Variable
import edu.jas.poly.{ExpVector, TermOrder}
import scas.base.BigInteger
import BigInteger.given

class PowerProduct(val variables: Variable*)(tord: TermOrder) extends scas.power.conversion.PowerProduct[ExpVector] {
  given instance: PowerProduct = this
  def newInstance(variables: Variable*) = new PowerProduct(variables*)(tord)
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
  def dependencyOnVariables(x: ExpVector) = x.dependencyOnVariables
  extension (x: ExpVector) {
    def projection(n: Int, m: Int) = ???
    def convert(from: scas.power.PowerProduct[ExpVector]) = ???
    def toCode(level: Level) = x.toString(variables.map(_.toString).toArray)
    def toMathML = ???
  }
  extension (x: ExpVector) def size = x.dependentVariables
}
