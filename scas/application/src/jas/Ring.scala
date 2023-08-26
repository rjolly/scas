package jas

import edu.jas.structure.RingElem

trait Ring[T <: RingElem[T]] extends impl.Ring[T] with scas.structure.ordered.Ring[T]
