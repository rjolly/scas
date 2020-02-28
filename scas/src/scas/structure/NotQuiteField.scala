package scas.structure

trait NotQuiteField[T] extends Ring[T] with
  def (x: T) / (y: T): T
  def (x: T) /:(y: T) = x / y
