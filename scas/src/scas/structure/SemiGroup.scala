package scas.structure

trait SemiGroup[T] extends Structure[T] {
  extension (x: T) {
    def multiply(y: T): T
    def * (y: T) = x.multiply(y)
  }
}
