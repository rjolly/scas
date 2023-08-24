package scas.structure

trait Ring[T] extends Ring.Impl[T] with AbelianGroup[T] with Monoid[T]

object Ring {
  trait Impl[T] extends AbelianGroup.Impl[T] with Monoid.Impl[T] {
    def characteristic: scas.base.BigInteger
    def apply(n: Long): T
  }
}
