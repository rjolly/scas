package scas.polynomial.gb

import scas.power.PowerProduct

class Pair[M : PowerProduct as pp](val i: Int, val j: Int, val m: M, val n: M, val scm: M) {
  def key = (scm, j, i)
  override def toString = "{" + i + ", " + j + "}, " + scm.show + ", " + reduction
  def reduction = if m < n then m | n else n | m
  def principal = if m < n then j else i
  def coprime = pp.coprime(m, n)
}
