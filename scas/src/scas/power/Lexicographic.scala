package scas.power

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends Lexicographic.Impl[N]

object Lexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new Conv[N](variables.map(~_)*)

  @nowarn("msg=New anonymous class definition will be duplicated at each inline site") inline def inlined[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*): Lexicographic[N] = new Lexicographic[N](variables.map(~_)*) {
    override def compare(x: Array[N], y: Array[N]) = {
      var i = length
      while i > 0 do {
        i -= 1
        if x(i) < y(i) then return -1
        if x(i) > y(i) then return 1
      }
      0
    }
    extension (x: Array[N]) override def multiply(y: Array[N]) = {
      val r = empty
      var i = 0
      while i <= length do {
        r(i) = x(i) + y(i)
        i += 1
      }
      r
    }
  }

  trait Impl[N : {Numeric, ClassTag}] extends ArrayPowerProductWithDegree[N] {
    def compare(x: Array[N], y: Array[N]) = {
      var i = length
      while i > 0 do {
        i -= 1
        if x.get(i) < y.get(i) then return -1
        if x.get(i) > y.get(i) then return 1
      }
      0
    }
  }

  class Conv[N : {Numeric, ClassTag}](val variables: Variable*) extends Impl[N] with PowerProduct.Conv[Array[N]] {
    given instance: Conv[N] = this
  }
}
