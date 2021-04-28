package scas.power.conversion

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.{ClassTagArray, Conversion, unary_~}
import scas.variable.Variable
import scas.power.Lexicographic.Impl

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](variables: Variable*) extends scas.power.Lexicographic[N](variables: _*) with PowerProduct[Array[N]]

object Lexicographic extends Impl {
  def apply[N : Numeric : ClassTag : ClassTagArray, U: Conversion[Variable]](degree: N)(variables: U*) = new Lexicographic[N](variables.map(~_): _*)
}
