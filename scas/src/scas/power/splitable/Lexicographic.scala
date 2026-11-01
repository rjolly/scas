package scas.power.splitable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic[N : {Numeric, ClassTag}](degree: N)(val variables: Variable*) extends ArrayPowerProduct[N] with scas.power.Lexicographic.Impl[N] {
  def newInstance(variables: Variable*) = new Lexicographic(degree)(variables*)
}

object Lexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new Lexicographic(degree)(variables.map(~_)*)
}
