package scas.structure.conversion

trait AbelianGroup[T] extends scas.structure.AbelianGroup[T] with Structure[T] {
  extension[U] (x: U)(using c: U => T) {
    def + (y: T) = c(x).add(y)
    def - (y: T) = c(x).subtract(y)
    def unary_- = super.unary_-(c(x))
  }
  extension (x: T) {
    def +[U](y: U)(using c: U => T) = x.add(c(y))
    def -[U](y: U)(using c: U => T) = x.subtract(c(y))
  }
  def abs[U](x: U)(using c: U => T) = super.abs(c(x))
}
