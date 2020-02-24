package jas

import scas.structure.ordered.Ring
import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory

class JasRing[T <: RingElem[T] : RingFactory] extends Ring[T] with
  def (x: T) + (y: T) = x.sum(y)
  def (x: T) - (y: T) = x.subtract(y)
  def (x: T) * (y: T) = x.multiply(y)
  def compare(x: T, y: T) = x.compareTo(y)
  def (x: T).isUnit = x.isUnit
  def zero = summon[RingFactory[T]].getZERO()
  def one = summon[RingFactory[T]].getONE()
