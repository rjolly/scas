package scas.structure

import scas.math.Equiv
import scas.prettyprint.{Show, MathObject}

trait Structure[T] extends Equiv[T] with Show[T] with MathObject with (T => T) {
  def apply(x: T) = x
  def random(numbits: Int)(using rnd: java.util.Random): T = ???
}
