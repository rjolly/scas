package scas.structure

trait AbelianGroup[T] extends Structure[T] with
  def (x: T) + (y: T): T
  def (x: T) +:(y: T) = x + y
  def (x: T) - (y: T): T
  def (x: T) -:(y: T) = x - y
  def (x: T).unary_- = zero - x
  def (x: T).abs = if (x.signum < 0) -x else x
  def (x: T).signum: Int
  def zero: T
