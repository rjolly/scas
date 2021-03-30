package scas.structure.conversion

trait Structure[T] extends scas.structure.Structure[T] with scas.math.conversion.Equiv[T] {
  def apply[U](x: U)(using c: U => T) = super.apply(c(x))
}
