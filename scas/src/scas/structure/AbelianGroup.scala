package scas.structure

import scala.language.experimental.into

trait AbelianGroup[T] extends Structure[T] {
  extension (x: into T) {
    def + (y: into T) = x.add(y)
    def - (y: into T) = x.subtract(y)
    def add(y: T): T
    def subtract(y: T): T
    def unary_- = zero - x
    def isZero = x >< zero
    def signum: Int
  }
  def abs(x: into T) = if (x.signum < 0) -x else x
  def zero: T
}
