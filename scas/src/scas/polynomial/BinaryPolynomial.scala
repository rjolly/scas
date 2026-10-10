package scas.polynomial

import scala.compiletime.deferred
import scala.annotation.targetName
import scas.power.compact.BinaryPowerProduct
import scas.base.BigInteger.given

trait BinaryPolynomial[T, C] extends ConvertablePolynomial[T, C, Int] {
  given pp: BinaryPowerProduct = deferred
  def apply(d: Int): T = this((pp.generator(d), ring.zero))
  override def equiv(x: T, y: T) = {
    if x.isDefining then equiv(zero, y)
    else if y.isDefining then equiv(x, zero)
    else super.equiv(x, y)
  }
  override def normalize(x: T) = {
    if (x.isDefining) then x
    else super.normalize(x)
  }
  override def s_polynomial(x: T, y: T) = {
    if x.isDefining then {
      if y.isDefining then ???
      else generator(x.index) * y
    } else {
      if y.isDefining then x * generator(y.index)
      else super.s_polynomial(x, y)
    }
  }
  extension (x: T) {
    def defining = this(x.index)
    def index = super.headPowerProduct(x).dependencyOnVariables(0)
    def isDefining = !x.isZero && x.headCoefficient.isZero
    override def headPowerProduct = {
      val m = pp.relaxed.convert(super.headPowerProduct(x))(pp)
      if x.isDefining then pp.relaxed.\(m)(2) else m
    }
    @targetName("coef") override def coefficient(y: T): T = this(x.coefficient(super.headPowerProduct(y)))
    override def coefficient(m: Array[Int]) = super.coefficient(x)(m)
    override def reduce(ys: T*) = {
      if x.isDefining then x
      else super.reduce(x)(ys.filterNot(_.isDefining)*)
    }
    override def reduce(strict: Boolean, tail: Boolean, ys: T*) = {
      if x.isDefining then x
      else super.reduce(x)(strict, tail, ys.filterNot(_.isDefining)*)
    }
  }
}
