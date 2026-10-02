package scas.power.offset.degree

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends ArrayPowerProduct[N] {
  def compare(x: Array[N], n: Int, y: Array[N], m: Int) = {
    if x.deg(n) < y.deg(m) then return -1
    if x.deg(n) > y.deg(m) then return 1
    val k = n * length
    val l = m * length
    var i = k
    var j = l
    while i < nbvars do {
      if x(i) > y(j) then return -1
      if x(i) < y(j) then return 1
      i += 1
      j += 1
    }
    0
  }
}

object DegreeReverseLexicographic {
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site") inline def inlined[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*): DegreeReverseLexicographic[N] = new DegreeReverseLexicographic[N](variables.map(~_)*) {
    override def compare(x: Array[N], n: Int, y: Array[N], m: Int) = {
      if x.deg(n) < y.deg(m) then return -1
      if x.deg(n) > y.deg(m) then return 1
      val k = n * length
      val l = m * length
      var i = k
      var j = l
      while i < nbvars do {
        if x(i) > y(j) then return -1
        if x(i) < y(j) then return 1
        i += 1
        j += 1
      }
      0
    }
    override def multiply(x: Array[N], n: Int, y: Array[N], z: Array[N]) = {
      val k = n * length
      var i = 0
      while i <= nbvars do {
        z(i + k) = x(i + k) + y(i)
        i += 1
      }
      z
    }
    extension (x: Array[N]) {
      override def deg(n: Int) = x(n * length + nbvars)
    }
  }
}
