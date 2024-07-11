package scas.util

extension [U](x: U)
  def unary_~[T](using c: U => T) = c(x)

extension [U](x: Seq[U])
  def unary_~[T](using c: U => T) = x.map(c)

type Conversion[T] = [X] =>> X => T

type ToFrags[T] = [X] =>> Fragable[X, T]
