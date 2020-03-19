package scas.structure

trait NotQuiteField[T] extends Ring[T] with
  def (x: T) / (y: T): T
