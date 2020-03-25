package scas.structure

trait NotQuiteField[T] extends Ring[T] {
  def (x: T) / (y: T): T
}
