package scas.power.offset

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends PowerProductWithDegree[N] {
  def newInstance(variables: Variable*) = new Lexicographic[N](variables*)

  def compare(x: Array[N], n: Int, y: Array[N], m: Int) = {
    val k = n * (length + 1)
    val l = m * (length + 1)
    var i = length + k
    var j = length + l
    while i > k do {
      i -= 1
      j -= 1
      if x(i) < y(j) then return -1
      if x(i) > y(j) then return 1
    }
    0
  }
}

object Lexicographic {
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site") inline def inlined[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*): Lexicographic[N] = new Lexicographic[N](variables.map(~_)*) {
    override def newInstance(variables: Variable*) = ???

    override def compare(x: Array[N], n: Int, y: Array[N], m: Int) = {
      val k = n * (length + 1)
      val l = m * (length + 1)
      var i = length + k
      var j = length + l
      while i > k do {
        i -= 1
        j -= 1
        if x(i) < y(j) then return -1
        if x(i) > y(j) then return 1
      }
      0
    }
    override def multiply(x: Array[N], n: Int, y: Array[N], z: Array[N]) = {
      val k = n * (length + 1)
      var i = 0
      while i <= length do {
        z(i + k) = x(i + k) + y(i)
        i += 1
      }
    }
  }
}
