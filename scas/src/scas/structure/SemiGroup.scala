package scas.structure

trait SemiGroup[T] extends Structure[T] with
  def (x: T) * (y: T): T
  def (x: T) *:(y: T) = x * y