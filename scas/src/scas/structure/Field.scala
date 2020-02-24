package scas.structure

trait Field[T] extends Ring[T] with
  def (x: T) / (y: T): T
  def (x: T) /:(y: T) = x / y
