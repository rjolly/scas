package scas.polynomial.gb

import scas.power.PowerProduct
import scas.base.BigInteger
import BigInteger.given

class SugarPair[M](using pp: PowerProduct[M])(i: Int, j: Int, m: M, n: M, scm: M, s: BigInteger) extends Pair(i, j, m, n, scm) {
  def skey = (s, scm, j, i)
  override def toString = "{" + i + ", " + j + "}, " + s.show + ", " + reduction
}
