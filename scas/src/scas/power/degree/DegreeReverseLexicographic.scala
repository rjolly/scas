package scas.power.degree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends scas.power.DegreeReverseLexicographic.Impl[N] with ArrayPowerProduct[N]
