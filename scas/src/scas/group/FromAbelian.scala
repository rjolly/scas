package scas.group

import scas.structure.{AbelianGroup, Group, Monoid}

trait FromAbelian[T] extends Group[T] {
  given group: AbelianGroup[T]
  override def one = group.zero
  extension (x: T) {
    def multiply(y: T) = group.add(x)(y)
  }
  def inverse(x: T) = group.unary_-(x)
  def equiv(x: T, y: T) = group.equiv(x, y)
  extension (x: T) {
    def toCode(level: Level) = group.toCode(x)(level)
    def toMathML = group.toMathML(x)
  }
  def toMathML = group.toMathML
}

object FromAbelian {
  class Conv[T](using val group: AbelianGroup[T]) extends FromAbelian[T] with Monoid.Conv[T] {
    given instance: Conv[T] = this
  }
}
