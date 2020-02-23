package scas

trait Show[T] with
  def (x: T).toCode(level: Int): String
  def (x: T).toCode: String = x.toCode(0)
