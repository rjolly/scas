package scas.structure.ordered

trait Ring[@specialized(Int, Long, Double) T] extends AbelianGroup[T] with scas.structure.Ring[T]
