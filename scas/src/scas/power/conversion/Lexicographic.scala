package scas.power.conversion

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.ClassTagArray
import scas.variable.Variable
import Variable.string2variable

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](variables: Variable*) extends scas.power.Lexicographic[N](variables: _*) with PowerProduct[Array[N]] {
  def this(variables: String*)(using DummyImplicit) = this(variables.map(string2variable): _*)
}
