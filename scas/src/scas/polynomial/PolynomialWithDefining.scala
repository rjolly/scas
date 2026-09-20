package scas.polynomial

import scala.annotation.targetName
import PolynomialWithDefining.Element
import scas.power.compact.PowerProduct
import scas.base.{BigInteger, ModInteger}
import BigInteger.given

class PolynomialWithDefining[T](using factory: BinaryPolynomial[T]) extends Polynomial[Element[T], Int, Array[Int]] {
  override given ring: ModInteger = factory.ring
  override given pp: PowerProduct = factory.pp.defining
  def apply(s: (Array[Int], Int)*) = Right(factory(s*))
  @targetName("fromPolynomial") def apply(p: T) = Right(p)
  override def normalize(x: Element[T]) = x match {
    case Right(p) => Right(factory.normalize(p))
    case Left(d) => Left(d)
  }
  override def s_polynomial(x: Element[T], y: Element[T]) = x match {
    case Right(p) => y match {
      case Right(q) => Right(factory.s_polynomial(p, q))
      case Left(d) => Right(p * factory.generator(d))
    }
    case Left(d) => y match {
      case Right(q) => Right(factory.generator(d) * q)
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
    override def reduce(ys: Element[T]*) = x match {
      case Right(p) => Right(p.reduce(ys.flatMap(_.toSeq)*))
      case Left(d) => Left(d)
    }
    override def reduce(strict: Boolean, tail: Boolean, ys: Element[T]*) = x match {
      case Right(p) => Right(p.reduce(strict, tail, ys.flatMap(_.toSeq)*))
      case Left(d) => Left(d)
    }
    def add(y: Element[T]) = ???
    def map(f: (Array[Int], Int) => (Array[Int], Int)) = ???
  }
}

object PolynomialWithDefining {
  type Element[T] = Either[Int, T]
}
