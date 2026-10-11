package scas.polynomial

import scala.compiletime.deferred
import scas.power.compact.BinaryPowerProduct
import scas.base.BigInteger.given

trait BinaryPolynomial[T, C] extends ConvertablePolynomial[T, C, Int] {
  given pp: BinaryPowerProduct = deferred
  def apply(d: Int): T = this((pp.generator(d), ring.zero))
  override def normalize(x: T) = {
    if (x.defining) then x
    else super.normalize(x)
  }
  override def s_polynomial(x: T, y: T) = {
    if x.defining then {
      if y.defining then ???
      else generator(x.index) * y
    } else {
      if y.defining then x * generator(y.index)
      else super.s_polynomial(x, y)
    }
  }
  extension (x: T) {
    def index = super.headPowerProduct(x).dependencyOnVariables(0)
    def defining = !x.isZero && x.headCoefficient.isZero
    override def headPowerProduct = {
      val m = pp.defining.convert(super.headPowerProduct(x))(pp)
      if x.defining then pp.defining.\(m)(2) else m
    }
    override def reduce(ys: T*) = {
      if x.defining then x
      else super.reduce(x)(ys.filterNot(_.defining)*)
    }
    override def reduce(strict: Boolean, tail: Boolean, ys: T*) = {
      if x.defining then x
      else super.reduce(x)(strict, tail, ys.filterNot(_.defining)*)
    }
  }
}
