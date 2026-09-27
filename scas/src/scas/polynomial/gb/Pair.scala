package scas.polynomial.gb

import scas.power.PowerProduct

open class Pair[M : PowerProduct](val i: Int, val j: Int, val reduction: Boolean, val principal: Int, val coprime: Boolean, val scm: M) {
  override def toString = "{" + i + ", " + j + "}, " + scm.show + ", " + reduction
}
