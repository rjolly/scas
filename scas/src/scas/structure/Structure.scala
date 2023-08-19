package scas.structure

import scas.math.Equiv
import scas.prettyprint.Show
import scas.util.{Conversion, unary_~}

trait Structure[T] extends Equiv[T] with Show[T] {
  def convert(x: T) = x
  def convert[U: Conversion[T]](x: U): T = convert(~x)
  def random(numbits: Int)(using rnd: java.util.Random): T = ???
  def math = tagged(toMathML)
  def toMathML: String
}

object Structure {
  trait Ops[T] extends Equiv.Ops[T] { this: Structure[T] =>
  }
}
