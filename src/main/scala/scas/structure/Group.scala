package scas.structure

import scas.Variable

trait Group[@specialized(Int, Long, Double) T] extends NotQuiteGroup[T] {
  override def isUnit(x: T) = true
}

object Group {
  implicit def apply[@specialized(Int, Long, Double) T](group: AbelianGroup[T]) = new Group[T] {
    val self: Group[T] = this
    override def one = group.zero
    def times(x: T, y: T) = group.plus(x, y)
    def inverse(x: T) = group.negate(x)
    def equiv(x: T, y: T) = group.equiv(x, y)
    override def convert(x: T) = group.convert(x)
    def apply(l: Long) = group(l)
    def random(numbits: Int)(implicit rnd: java.util.Random) = group.random(numbits)
    override def toCode(x: T, precedence: Int) = group.toCode(x, precedence)
    def toMathML(x: T) = group.toMathML(x)
    def toMathML = group.toMathML
    def function(x: T, a: Variable) = group.function(x, a)
  }
}
