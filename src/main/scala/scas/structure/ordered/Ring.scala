package scas.structure.ordered

trait Ring[@specialized(Int, Long) T] extends AbelianGroup[T] with scas.structure.Ring[T]
