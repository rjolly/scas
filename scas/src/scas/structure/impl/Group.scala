package scas.structure.impl

trait Group[T] extends NotQuiteGroup[T] {
  extension (x: T) {
    def isUnit = true
  }
}

object Group {
  trait FromAbelian[T](using group: AbelianGroup[T]) extends Group[T] {
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
}
