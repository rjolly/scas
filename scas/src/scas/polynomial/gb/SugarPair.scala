package scas.polynomial.gb

import scas.power.PowerProduct
import scas.base.BigInteger
import BigInteger.given

class SugarPair[M : PowerProduct](i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M, s: BigInteger) extends Pair(i, j, reduction, principal, coprime, scm) {
  def skey = (s, scm, j, i)
  override def toString = "{" + i + ", " + j + "}, " + s.show + ", " + reduction
}
