package scas.structure

trait NotQuiteField[T] extends Ring[T] {
  extension (x: T) {
    def divide(y: T): T
    inline def / (y: T) = x.divide(y)
  }
}
