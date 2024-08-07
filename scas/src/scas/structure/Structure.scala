package scas.structure

import scas.math.Equiv
import scas.prettyprint.Show

trait Structure[T] extends Equiv[T] with Show[T] {
  def random(numbits: Int)(using rnd: java.util.Random): T = ???
  def fenced(s: String) = s"($s)"
  export scas.prettyprint.Level
  extension (x: T) {
    def toCode(level: Level): String
    def show = x.toCode(Level.Addition)
  }
  def toMathML: String
}
