package scas.structure

trait AbelianGroup[T] extends Structure[T] with
  def (x: T) + (y: T): T
  def (x: T) +: (y: T) = x + y
  def (x: T) - (y: T): T
  def (x: T) -: (y: T) = x - y
  def zero: T
