package jas.conversion

import edu.jas.structure.RingElem

trait Ring[T <: RingElem[T]] extends jas.Ring[T] with scas.structure.ordered.Ring[T]
