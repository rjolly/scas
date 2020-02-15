package scas.math

trait Equiv[T] extends scala.math.Equiv[T] with
  def (x: T) >< (y: T) = equiv(x, y)
  def (x: T) <> (y: T) = !equiv(x, y)
