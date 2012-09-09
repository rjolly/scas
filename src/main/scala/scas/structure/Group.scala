package scas.structure

trait Group[T] extends NotQuiteGroup[T] {
  override def isUnit(x: T) = true
}

object Group {
  def apply[T](group: AbelianGroup[T]) = new Group[T] {
    override def one = group.zero
    def times(x: T, y: T) = group.plus(x, y)
    def inverse(x: T) = group.negate(x)
    def compare(x: T, y: T) = group.compare(x, y)
    def convert(x: T) = group.convert(x)
    def apply(l: Long) = group(l)
    def random(numbits: Int)(implicit rnd: java.util.Random) = group.random(numbits)
    override def toCode(x: T, precedence: Int) = group.toCode(x, precedence)
    def toMathML(x: T) = group.toMathML(x)
    def toMathML = group.toMathML
  }
}
