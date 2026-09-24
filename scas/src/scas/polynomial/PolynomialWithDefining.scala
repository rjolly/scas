package scas.polynomial

import scala.annotation.targetName
import PolynomialWithDefining.Element
import scas.power.compact.PowerProduct
import scas.base.{BigInteger, ModInteger}
import BigInteger.given

class PolynomialWithDefining[T](using factory: BinaryPolynomial[T]) extends MutablePolynomial[Element[T], Int, Array[Int]] {
  override given ring: ModInteger = factory.ring
  override given pp: PowerProduct = factory.pp.defining
  def apply(s: (Array[Int], Int)*) = Right(factory(s*))
  @targetName("fromPolynomial") def apply(p: T) = Right(p)
  def unmodifiable(x: Element[T]) = x match {
    case Right(p) => Right(factory.unmodifiable(p))
    case Left(_) => x
  }
  def modifiable(x: Element[T]) = x match {
    case Right(p) => Right(factory.modifiable(p))
    case Left(_) => x
  }
  override def normalize(x: Element[T]) = {
    if (x.isRight) then super.normalize(x)
    else x
  }
  override def s_polynomial(x: Element[T], y: Element[T]) = x match {
    case Right(_) => y match {
      case Right(_) => super.s_polynomial(x, y)
      case Left(d) => x * generator(d)
    }
    case Left(d) => y match {
      case Right(_) => generator(d) * y
      case Left(_) => ???
    }
  }
  extension (x: Element[T]) {
    def iterator = ???
    def size = ???
    def head = ???
    def last = ???
    override def headPowerProduct = x match {
      case Right(p) => pp.convert(p.headPowerProduct)(factory.pp)
      case Left(d) => pp.convert(factory.generator(d).headPowerProduct)(factory.pp) \ 2
    }
    override def reduce(ys: Element[T]*) = {
      if (x.isRight) then super.reduce(x)(ys.filter(_.isRight)*)
      else x
    }
    override def reduce(strict: Boolean, tail: Boolean, ys: Element[T]*) = {
      if (x.isRight) then super.reduce(x)(strict, tail, ys.filter(_.isRight)*)
      else x
    }
    def add(y: Element[T]) = ???
    def map(f: (Array[Int], Int) => (Array[Int], Int)) = ???
  }
}

object PolynomialWithDefining {
  type Element[T] = Either[Int, T]
}
