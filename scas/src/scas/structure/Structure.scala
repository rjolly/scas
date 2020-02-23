package scas.structure

import scas.Show
import scas.math.Equiv

trait Structure[T] extends Equiv[T] with Show[T]
  def random(numbits: Int)(using rnd: java.util.Random): T = ???
  def (x: T).toCode(level: Int) = x.toString
