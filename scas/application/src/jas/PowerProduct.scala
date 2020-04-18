package jas

import scas.variable.Variable
import edu.jas.poly.ExpVector

class PowerProduct(val variables: Variable*) extends scas.power.PowerProduct[ExpVector] {
  def one = ExpVector.create(length)
  def generator(n: Int) = ExpVector.create(length, n, 1)
  def degree(x: ExpVector) = x.degree
  def gcd(x: ExpVector, y: ExpVector) = x.gcd(y)
  def lcm(x: ExpVector, y: ExpVector) = x.lcm(y)
  def (x: ExpVector) * (y: ExpVector) = x.sum(y)
  def (x: ExpVector) / (y: ExpVector) = x.subtract(y)
  def (x: ExpVector) | (y: ExpVector) = x.divides(y)
  def compare(x: ExpVector, y: ExpVector) = x.compareTo(y)
  def dependencyOnVariables(x: ExpVector) = x.dependencyOnVariables
  def (x: ExpVector).projection(n: Int) = ???
  def (x: ExpVector).toCode(level: Level) = x.toString(variables.map(_.toString).toArray)
  def (x: ExpVector).toMathML = ???
  def (x: ExpVector).convert(from: Variable*) = ???
  def size(x: ExpVector) = x.dependentVariables
}
