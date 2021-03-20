package scas.structure.commutative

trait Residue[T: UniqueFactorizationDomain] extends UniqueFactorizationDomain[T] {
  def ring = UniqueFactorizationDomain[T]
  extension (x: T) {
    def signum = ring.signum(x)
    def add(y: T) = this(ring.+(x)(y))
    def subtract(y: T) = this(ring.-(x)(y))
    def multiply(y: T) = this(ring.*(x)(y))
  }
  def equiv(x: T, y: T) = ring.equiv(x, y)
  extension (x: T) {
    def toCode(level: Level) = ring.toCode(x)(level)
    def toMathML = ring.toMathML(x)
  }
  def zero = ring.zero
  def one = ring.one
}
