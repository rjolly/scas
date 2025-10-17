package scas.power.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic[N : {Numeric, ClassTag}](variables: Variable*) extends ArrayPowerProduct[N](variables*) with scas.power.Lexicographic.Impl[N]

object Lexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new Lexicographic[N](variables.map(~_)*)
}
