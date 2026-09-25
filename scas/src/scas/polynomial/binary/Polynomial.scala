package scas.polynomial.binary

import scala.annotation.targetName
import scas.structure.Ring
import scas.power.compact.PowerProduct
import scas.polynomial.PolynomialWithDefining
import scas.base.BigInteger.given
import Polynomial.Element

class Polynomial[T, C](using factory: BinaryPolynomial[T, C]) extends PolynomialWithDefining[Element[T], C, Array[Int]] {
  override given ring: Ring[C] = factory.ring
  override given pp: PowerProduct = factory.pp.defining
  def apply(s: (Array[Int], C)*) = Right(factory(s*))
  @targetName("fromPolynomial") def apply(p: T) = Right(p)
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
    def map(f: (Array[Int], C) => (Array[Int], C)) = x match {
      case Right(p) => Right(p.map(f))
      case Left(_) => ???
    }
  }
}

object Polynomial {
  type Element[T] = Either[Int, T]
}
