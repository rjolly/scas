package scas.group

import scas.structure.{AbelianGroup, Group}

class FromAbelian[T](using val group: AbelianGroup[T]) extends Group[T] {
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
