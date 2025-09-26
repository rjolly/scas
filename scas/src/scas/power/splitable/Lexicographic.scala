package scas.power.splitable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic[N : {Numeric, ClassTag}](variables: Variable*) extends scas.power.Lexicographic[N](variables*) with PowerProduct[Array[N]] {
  def newInstance(variables: Variable*) = new Lexicographic[N](variables*)
}

object Lexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new Lexicographic[N](variables.map(~_)*)
}
