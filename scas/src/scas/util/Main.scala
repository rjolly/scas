package scas.util

extension [U](x: U)
  def unary_~[T](using c: U => T) = c(x)

type Conversion[T] = [X] =>> X => T

type ToFrags[T] = [X] =>> Fragable[X, T]

type ClassTagArray[N] = scala.reflect.ClassTag[Array[N]]
