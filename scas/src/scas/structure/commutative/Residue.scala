package scas.structure.commutative

abstract class Residue[T: UniqueFactorizationDomain] extends UniqueFactorizationDomain[T] {
  def ring = UniqueFactorizationDomain[T]
  def (x: T).signum = ring.extension_signum(x)
  def (x: T) + (y: T) = this(ring.extension_+(x)(y))
  def (x: T) - (y: T) = this(ring.extension_-(x)(y))
  def (x: T) * (y: T) = this(ring.extension_*(x)(y))
  def equiv(x: T, y: T) = ring.equiv(x, y)
  def (x: T).toCode(level: Level) = ring.extension_toCode(x)(level)
  def (x: T).toMathML = ring.extension_toMathML(x)
  def zero = ring.zero
  def one = ring.one
}
