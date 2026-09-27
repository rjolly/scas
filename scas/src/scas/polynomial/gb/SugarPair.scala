package scas.polynomial.gb

import scas.base.BigInteger

class SugarPair[M](i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M, val s: BigInteger) extends Pair(i, j, reduction, principal, coprime, scm)
