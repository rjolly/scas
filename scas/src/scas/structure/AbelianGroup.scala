package scas.structure

trait AbelianGroup[T] extends Structure[T] with
  def (x: T) + (y: T): T
  def (x: T) - (y: T): T
  def (x: T).unary_- = zero - x
  def abs(x: T) = if (signum(x) < 0) -x else x
  def signum(x: T): Int
  def zero: T
