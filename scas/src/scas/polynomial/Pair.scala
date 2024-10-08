package scas.polynomial

import scas.power.PowerProduct

class Pair[M](using pp: PowerProduct[M])(val i: Int, val j: Int, val m: M, val n: M, val scm: M) {
  def key = (scm, j, i)
  override def toString = "{" + i + ", " + j + "}, " + scm.show + ", " + reduction
  def reduction = if (m < n) m | n else n | m
  def principal = if (m < n) j else i
  def coprime = pp.coprime(m, n)
}
