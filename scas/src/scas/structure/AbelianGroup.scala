package scas.structure

trait AbelianGroup[T] extends Structure[T] {
  def (x: T) + (y: T): T
  def (x: T) - (y: T): T
  def (x: T).unary_- = zero - x
  def abs(x: T) = if (x.signum < 0) -x else x
  def (x: T).signum: Int
  def zero: T
  def (x: T).isZero = x >< zero
}
