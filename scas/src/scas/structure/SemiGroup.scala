package scas.structure

trait SemiGroup[T] with
  def (x: T) * (y: T): T
  def (x: T) *: (y: T) = x * y
