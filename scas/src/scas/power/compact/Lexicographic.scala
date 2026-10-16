package scas.power.compact

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

open class Lexicographic(val shift: Int)(val variables: Variable*) extends Lexicographic.Impl

object Lexicographic {
  def binary = scas.power.compact.binary.Lexicographic
  def apply[S : Conversion[Variable]](shift: Int)(variables: S*) = new Lexicographic(shift)(variables.map(~_)*)

  trait Impl extends PowerProduct {
    def compare(x: Array[Int], y: Array[Int]) = {
      var i = length
      while i > 0 do {
        i -= 1
        if x(i) < y(i) then return -1
        if x(i) > y(i) then return 1
      }
      0
    }
  }
}
