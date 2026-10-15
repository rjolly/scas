package scas.power.sparse.degree

import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends PowerProduct[N] with scas.power.sparse.DegreeReverseLexicographic.Impl[N]

object DegreeReverseLexicographic {
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site") inline def inlined[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*): DegreeReverseLexicographic[N] = new DegreeReverseLexicographic[N](variables.map(~_)*) {
    override def compare(x: Array[N], y: Array[N]) = {
      if x.deg < y.deg then return -1
      if x.deg > y.deg then return 1
      var i = 0
      var j = 0
      while i < x.size && j < y.size do {
        if x(i) < y(j) then return -1
        if x(i) > y(j) then return 1
        if x.get(i) > y.get(j) then return -1
        if x.get(i) < y.get(j) then return 1
        i += 1
        j += 1
      }
      if i < x.size then return -1
      if j < y.size then return 1
      0
    }
    extension (x: Array[N]) {
      override def multiply(y: Array[N]) = {
        val buffer = emptybuffer
        var i = 0
        var j = 0
        while i < x.size && j < y.size do {
          if x(i) < y(j) then {
            buffer += ((x(i), x.get(i)))
            i += 1
          } else if x(i) > y(j) then {
            buffer += ((y(j), y.get(j)))
            j += 1
          } else {
            buffer += ((x(i), x.get(i) + y.get(j)))
            i += 1
            j += 1
          }
        }
        while i < x.size do {
          buffer += ((x(i), x.get(i)))
          i += 1
        }
        while j < y.size do {
          buffer += ((y(j), y.get(j)))
          j += 1
        }
        buffer.withdeg(x.deg + y.deg).array
      }
      override def deg = x.get(x.size)
    }
  }
}
