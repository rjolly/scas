package scas.power

import scas.math.Numeric
import scas.variable.Variable

abstract class ArrayPowerProductWithDegree[N : Numeric : ClassTag : ClassTagArray] extends ArrayPowerProduct[N] {
  val one = IArray.unsafeFromArray(mone)
  def mone = new Array[N](length + 1)
  def generator(n: Int) = {
    val r = mone
    for (i <- 0 to length) r(i) = numeric.fromInt(if (i == n || i == length) 1 else 0)
    IArray.unsafeFromArray(r)
  }
  def degree(x: IArray[N]) = x(length).toLong
  def gcd(x: IArray[N], y: IArray[N]): IArray[N] = {
    val r = mone
    for (i <- 0 until length) {
      r(i) = numeric.min(x(i), y(i))
      r(length) += r(i)
    }
    IArray.unsafeFromArray(r)
  }
  def lcm(x: IArray[N], y: IArray[N]): IArray[N] = {
    val r = mone
    for (i <- 0 until length) {
      r(i) = numeric.max(x(i), y(i))
      r(length) += r(i)
    }
    IArray.unsafeFromArray(r)
  }
  extension (x: IArray[N]) def * (y: IArray[N]) = {
    val r = mone
    var i = 0
    while (i <= length) {
      r(i) = x(i) + y(i)
      i += 1
    }
    IArray.unsafeFromArray(r)
  }
  extension (x: IArray[N]) def / (y: IArray[N]) = {
    val r = mone
    for (i <- 0 to length) {
      assert (x(i) >= y(i))
      r(i) = x(i) - y(i)
    }
    IArray.unsafeFromArray(r)
  }
  extension (x: IArray[N]) def | (y: IArray[N]) = {
    var i = 0
    while (i < length) {
      if (x(i) > y(i)) return false
      i += 1
    }
    true
  }
  extension (x: IArray[N]) def projection(n: Int) = {
    val r = mone
    for (i <- 0 to length) r(i) = if (i == n || i == length) x(n) else numeric.zero
    IArray.unsafeFromArray(r)
  }
  extension (x: IArray[N]) def convert(from: Variable*) = {
    val r = mone
    val index = from.map(a => variables.indexOf(a))
    for (i <- 0 until x.length - 1) if (x(i) > numeric.zero) {
      val c = index(i)
      assert (c > -1)
      r(c) = x(i)
    }
    r(length) = x(x.length - 1)
    IArray.unsafeFromArray(r)
  }
  extension (x: IArray[N]) def get(i: Int) = x(i)
}
