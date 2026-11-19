package scas.power

import scala.language.experimental.inlineTraits
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic[N : {Numeric, ClassTag}](degree: N)(val variables: Variable*) extends Lexicographic.Impl[N]

object Lexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new Conv(degree)(variables.map(~_)*)
  def degreeReverse = scas.power.degree.DegreeReverseLexicographic

  inline def inlined[N : {Specialized, Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new Spec(degree)(variables.map(~_)*) {
  }

  inline trait Spec[N : {Specialized, Numeric}](degree: N)(val variables: Variable*) extends Impl[N] {
    override def compare(x: Array[N], y: Array[N]) = {
      var i = nbvars
      while i > 0 do {
        i -= 1
        if x(i) < y(i) then return -1
        if x(i) > y(i) then return 1
      }
      0
    }
    override def multiply(x: Array[N], y: Array[N], z: Array[N]) = {
      var i = 0
      while i < nbvars do {
        z(i) = x(i) + y(i)
        i += 1
      }
      z
    }
  }

  trait Impl[N : Numeric] extends ArrayPowerProduct[N] {
    def compare(x: Array[N], y: Array[N]) = {
      var i = nbvars
      while i > 0 do {
        i -= 1
        if x.get(i) < y.get(i) then return -1
        if x.get(i) > y.get(i) then return 1
      }
      0
    }
  }

  class Conv[N : {Numeric, ClassTag}](degree: N)(variables: Variable*) extends Lexicographic(degree)(variables*) with PowerProduct.Conv[Array[N]] {
    given instance: Conv[N] = this
  }
}
