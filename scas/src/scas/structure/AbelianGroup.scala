package scas.structure

trait AbelianGroup[T] with
  def (x: T) + (y: T): T
  def (x: T) +: (y: T) = x + y
  def (x: T) - (y: T): T
  def (x: T) -: (y: T) = x - y
  def (x: T) isZero: Boolean
  def zero: T
