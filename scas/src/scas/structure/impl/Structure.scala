package scas.structure.impl

import scas.math.impl.Equiv
import scas.prettyprint.Show

trait Structure[T] extends Equiv[T] with Show[T] {
  def convert(x: T) = x
  def random(numbits: Int)(using rnd: java.util.Random): T = ???
  def math = tagged(toMathML)
  def toMathML: String
}