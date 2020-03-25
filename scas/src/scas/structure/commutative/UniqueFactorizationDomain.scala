package scas.structure.commutative

trait UniqueFactorizationDomain[T] extends scas.structure.NotQuiteField[T] {
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  def (x: T) / (y: T) = { val (q, _) = x /%y ; q }
  def (x: T) % (y: T) = { val (_, r) = x /%y ; r }
  def (x: T) /%(y: T): (T, T)
  def (x: T) | (y: T) = (y % x) >< zero
}

object UniqueFactorizationDomain {
  def apply[T : UniqueFactorizationDomain] = summon[UniqueFactorizationDomain[T]]
}
