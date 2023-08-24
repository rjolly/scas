package scas.structure

import scas.math.Equiv
import scas.prettyprint.Show
import scas.util.{Conversion, unary_~}

trait Structure[T] extends Structure.Impl[T] with Equiv[T] {
  def convert[U: Conversion[T]](x: U) = super.convert(~x)
}

object Structure {
  trait Impl[T] extends Equiv.Impl[T] with Show[T] {
    def convert(x: T) = x
    def random(numbits: Int)(using rnd: java.util.Random): T = ???
    def math = tagged(toMathML)
    def toMathML: String
  }
}
