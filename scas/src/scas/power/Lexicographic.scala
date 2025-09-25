package scas.power

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

open class Lexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends ArrayPowerProductWithDegree[N] {
  def newInstance(variables: Variable*) = new Lexicographic[N](variables*)

  def compare(x: Array[N], y: Array[N]) = {
    var i = length
    while i > 0 do {
      i -= 1
      if x(i) < y(i) then return -1
      if x(i) > y(i) then return 1
    }
    0
  }
}

object Lexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new Conv[N](variables.map(~_)*)

  @nowarn("msg=New anonymous class definition will be duplicated at each inline site") inline def inlined[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*): Lexicographic[N] = new Lexicographic[N](variables.map(~_)*) {
    override def newInstance(variables: Variable*) = ???

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

  class Conv[N : {Numeric, ClassTag}](variables: Variable*) extends Lexicographic[N](variables*) with PowerProduct.Conv[Array[N]] {
    given instance: Conv[N] = this
  }
}
