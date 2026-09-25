package scas.polynomial.binary

import scala.annotation.targetName
import PolynomialWithDefining.Element
import scas.power.compact.PowerProduct
import scas.polynomial.{BinaryPolynomial, MutablePolynomial}
import scas.base.{BigInteger, ModInteger}
import BigInteger.given

class PolynomialWithDefining[T](using factory: BinaryPolynomial[T]) extends scas.polynomial.PolynomialWithDefining[Element[T], Int, Array[Int]] with MutablePolynomial[Element[T], Int, Array[Int]] {
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
  extension (x: Element[T]) {
    def index = x match {
      case Right(_) => ???
      case Left(d) => d
    }
    def defining = x.isLeft
    def iterator = x match {
      case Right(p) => p.iterator
      case Left(_) => ???
    }
    def size = x match {
      case Right(p) => p.size
      case Left(_) => ???
    }
    def head = x match {
      case Right(p) => p.head
      case Left(_) => ???
    }
    def last = x match {
      case Right(p) => p.last
      case Left(_) => ???
    }
    override def headPowerProduct = x match {
      case Right(_) => pp.convert(super.headPowerProduct(x))(factory.pp)
      case Left(d) => pp.convert(super.headPowerProduct(generator(d)))(factory.pp) \ 2
    }
    def add(y: Element[T]) = x match {
      case Right(p) => y match {
        case Right(q) => Right(p + q)
        case Left(_) => ???
      }
      case Left(_) => ???
    }
    def map(f: (Array[Int], Int) => (Array[Int], Int)) = x match {
      case Right(p) => Right(p.map(f))
      case Left(_) => ???
    }
  }
}

object PolynomialWithDefining {
  type Element[T] = Either[Int, T]
}
