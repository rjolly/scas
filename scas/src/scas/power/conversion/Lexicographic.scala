package scas.power.conversion

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

class Lexicographic[N : Numeric : ClassTag](variables: Variable*) extends scas.power.Lexicographic[N](variables*) with PowerProduct[Array[N]] {
  given instance: Lexicographic[N] = this
}
