package scas.structure

trait NotQuiteField[T] extends Ring[T] {
  extension (x: T) {
    def divide(y: T): T
    def / (y: T) = x.divide(y)
  }
}
