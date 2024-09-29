package scas.adapter.jas.conversion

import edu.jas.structure.RingElem

trait Ring[T <: RingElem[T]] extends scas.adapter.jas.Ring[T] with scas.structure.ordered.conversion.Ring[T]
