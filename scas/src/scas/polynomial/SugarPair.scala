package scas.polynomial

import scas.power.ArrayPowerProduct
import scas.base.BigInteger
import BigInteger.given

class SugarPair[N](using pp: ArrayPowerProduct[N])(i: Int, j: Int, m: Array[N], n: Array[N], scm: Array[N], s: BigInteger) extends Pair(i, j, m, n, scm) {
  def skey = (s, scm, j, i)
  override def toString = "{" + i + ", " + j + "}, " + s.show + ", " + reduction
}
