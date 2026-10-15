package scas.power.sparse

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.boundary, boundary.break
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.ArrayPowerProduct

trait PowerProduct[N : {Numeric as numeric, ClassTag}] extends ArrayPowerProduct[N] {
  override def length = 0
  def emptybuffer = ArrayBuffer.empty[(N, N)]
  override def generator(n: Int) = generator(n, emptybuffer).array
  def generator(n: Int, buffer: ArrayBuffer[(N, N)]) = {
    buffer += ((numeric.fromInt(n), numeric.one))
    buffer
  }
  override def gcd(x: Array[N], y: Array[N]) = gcd(x, y, emptybuffer).array
  def gcd(x: Array[N], y: Array[N], buffer: ArrayBuffer[(N, N)]) = {
    for i <- 0 until x.size do {
      val j = y.indexOf(x(i))
      if j > -1 then {
        buffer += ((x(i), numeric.min(x.get(i), y.get(j))))
      }
    }
    buffer
  }
  override def lcm(x: Array[N], y: Array[N]) = lcm(x, y, emptybuffer).array
  def lcm(x: Array[N], y: Array[N], buffer: ArrayBuffer[(N, N)]) = {
    var i = 0
    var j = 0
    while i < x.size && j < y.size do {
      if x(i) < y(j) then {
        buffer += ((x(i), x.get(i)))
        i += 1
      } else if x(i) > y(j) then {
        buffer += ((y(j), y.get(j)))
        j += 1
      } else {
        buffer += ((x(i), numeric.max(x.get(i), y.get(j))))
        i += 1
        j += 1
      }
    }
    while i < x.size do {
      buffer += ((x(i), x.get(i)))
      i += 1
    }
    while j < y.size do {
      buffer += ((y(j), y.get(j)))
      j += 1
    }
    buffer
  }
  def multiply(x: Array[N], y: Array[N], buffer: ArrayBuffer[(N, N)]) = {
    var i = 0
    var j = 0
    while i < x.size && j < y.size do {
      if x(i) < y(j) then {
        buffer += ((x(i), x.get(i)))
        i += 1
      } else if x(i) > y(j) then {
        buffer += ((y(j), y.get(j)))
        j += 1
      } else {
        buffer += ((x(i), x.get(i) + y.get(j)))
        i += 1
        j += 1
      }
    }
    while i < x.size do {
      buffer += ((x(i), x.get(i)))
      i += 1
    }
    while j < y.size do {
      buffer += ((y(j), y.get(j)))
      j += 1
    }
    buffer
  }
  def divide(x: Array[N], y: Array[N], buffer: ArrayBuffer[(N, N)]) = {
    for i <- 0 until x.size do {
      val j = y.indexOf(x(i))
      val c = if j > -1 then y.get(j) else numeric.zero
      assert (x.get(i) >= c)
      if x.get(i) > c then {
        buffer += ((x(i), x.get(i) - c))
      }
    }
    buffer
  }
  def projection(x: Array[N], n: Int, m: Int, buffer: ArrayBuffer[(N, N)]) = {
    for i <- 0 until x.size do {
      if numeric.toInt(x(i)) >= n && numeric.toInt(x(i)) < m then {
        buffer += ((x(i), x.get(i)))
      }
    }
    buffer
  }
  def convert(x: Array[N], from: ArrayPowerProduct[N], buffer: ArrayBuffer[(N, N)]) = {
    val index = from.variables.map(a => variables.indexOf(a))
    for i <- 0 until x.size do {
      val c = index(numeric.toInt(x(i)))
      assert (c > -1)
      buffer += ((numeric.fromInt(c), x.get(i)))
    }
    buffer.sortBy((s, _) => s)
  }
  extension (x: Array[N]) {
    override def multiply(y: Array[N]) = this.multiply(x, y, emptybuffer).array
    override def divide(y: Array[N]) = this.divide(x, y, emptybuffer).array
    override def factorOf(y: Array[N]) = boundary {
      for i <- 0 until x.size do {
        val j = y.indexOf(x(i))
        val c = if j > -1 then y.get(j) else numeric.zero
        if x.get(i) > c then break(false)
      }
      true
    }
    override def projection(n: Int, m: Int) = this.projection(x, n, m, emptybuffer).array
    override def convert(from: ArrayPowerProduct[N]): Array[N] = this.convert(x, from, emptybuffer).array
    override def dependencyOnVariables = x.map(n => numeric.toInt(n))
    override def toCode(level: Level, times: String) = {
      var s = "1"
      var m = 0
      for i <- 0 until x.size do {
        val a = variables(numeric.toInt(x(i)))
        val b = x.get(i)
        val t = if b >< numeric.one then a.toString else s"$a\\$b"
        s = if m == 0 then t else s + times + t
        m += 1
      }
      s
    }
    override def toMathML(times: String) = {
      var s = "<cn>1</cn>"
      var m = 0
      for i <- 0 until x.size do {
        val a = variables(numeric.toInt(x(i)))
        val b = x.get(i)
        val t = if b >< numeric.one then a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
        s = if m == 0 then t else s"<apply><$times/>$s$t</apply>"
        m += 1
      }
      s
    }
    def middle = x.length >> 1
    override def size = x.middle
    override def deg = {
      var d = numeric.zero
      for i <- 0 until x.size do d += x.get(i)
      d
    }
    def indexOf(elem: N): Int = {
      val n = x.indexOf(elem, 0, x.size)
      if n == x.size || x(n) > elem then -1 else n
    }
    @tailrec final def indexOf(elem: N, i: Int, j: Int): Int = {
      val n = (i + j) >> 1
      if n == j then n else {
        val s = x(n)
        if s > elem then x.indexOf(elem, i, n)
        else if s < elem then x.indexOf(elem, n + 1, j)
        else n
      }
    }
    override def get(i: Int) = x(i + x.middle)
    override def set(i: Int, c: N) = ???
  }
  extension (buffer: ArrayBuffer[(N, N)]) {
    def array = (buffer.map(_._1) ++ buffer.map(_._2)).toArray
  }
}
