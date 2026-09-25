package scas.polynomial.binary

import Polynomial.Element

class MutablePolynomial[T, C](using factory: MutableBinaryPolynomial[T, C]) extends Polynomial[T, C] with scas.polynomial.MutablePolynomial[Element[T], C, Array[Int]] {
  def unmodifiable(x: Element[T]) = x match {
    case Right(p) => Right(factory.unmodifiable(p))
    case Left(_) => x
  }
  def modifiable(x: Element[T]) = x match {
    case Right(p) => Right(factory.modifiable(p))
    case Left(_) => x
  }
  extension (x: Element[T]) {
    override def subtract(m: Array[Int], c: C, y: Element[T]) = x match {
      case Right(p) => y match {
        case Right(q) => Right(p.subtract(m, c, q))
        case Left(_) => ???
      }
      case Left(_) => ???
    }
    override def multiplyRight(c: C) = x match {
      case Right(p) => Right(p%* c)
      case Left(_) => ???
    }
  }
}
