package scas.power.sparse

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends PowerProduct[N] {
  def compare(x: Array[N], y: Array[N]) = {
    var i = x.size
    var j = y.size
    while i > 0 && j > 0 do {
      i -= 1
      j -= 1
      if x(i) < y(j) then return -1
      if x(i) > y(j) then return 1
      if x.get(i) < y.get(j) then return -1
      if x.get(i) > y.get(j) then return 1
    }
    if j > 0 then return -1
    if i > 0 then return 1
    0
  }
}

object Lexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new Lexicographic[N](variables.map(~_)*)
}
