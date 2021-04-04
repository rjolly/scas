package scas.util

import scala.reflect.ClassTag

trait Fragable[S, T : ClassTag] extends (S => Array[T]):
  def apply(x: S) = x.toFrags
  extension (x: S)
    def toFrags: Array[T]

object Fragable:
  given [U : Conversion[T], T : ClassTag]: Fragable[U, T] with
    extension (x: U)
      def toFrags = Array(~x)
  given [T : ClassTag]: Fragable[EmptyTuple, T] with
    extension (x: EmptyTuple)
      def toFrags = Array()
  given [A : ToFrags[T], B <: Tuple : ToFrags[T], T : ClassTag]: Fragable[A *: B, T] with
    extension (x: A *: B)
      def toFrags = x.head.toFrags ++ x.tail.toFrags
