package scas.structure

import scas.math.conversion.Equiv
import scas.prettyprint.Show

trait Structure[T] extends Equiv[T] with Show[T] {
  def apply[U](x: U)(using c: U => T): T = this(c(x))
  def apply(x: T) = x
  def random(numbits: Int)(using rnd: java.util.Random): T = ???
  def math = Show.math(toMathML)
  def toMathML: String
}

object Structure {
  def apply[T : Structure] = summon[Structure[T]]
}
