package scas.structure.impl

trait SemiGroup[T] extends Structure[T] {
  extension (x: T) {
    def multiply(y: T): T
    inline def * (y: T) = x.multiply(y)
  }
}
