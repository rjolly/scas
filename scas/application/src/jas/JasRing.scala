package jas

import scas.math.Ordering
import scas.structure.Ring
import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory

class JasRing[T <: RingElem[T] : RingFactory] extends Ring[T] with Ordering[T] with
  def (x: T) + (y: T) = x.sum(y)
  def (x: T) - (y: T) = x.subtract(y)
  def (x: T) * (y: T) = x.multiply(y)
  def compare(x: T, y: T) = x.compareTo(y)
  def zero = summon[RingFactory[T]].getZERO()
  def one = summon[RingFactory[T]].getONE()
