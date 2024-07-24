package scas.polynomial

import scas.power.ArrayPowerProduct

class Pair[N](using pp: ArrayPowerProduct[N])(val i: Int, val j: Int, val m: Array[N], val n: Array[N], val scm: Array[N]) {
  def key = (scm, j, i)
  override def toString = "{" + i + ", " + j + "}, " + scm.show + ", " + reduction
  def reduction = if (m < n) m | n else n | m
  def principal = if (m < n) j else i
  def coprime = pp.coprime(m, n)
}
