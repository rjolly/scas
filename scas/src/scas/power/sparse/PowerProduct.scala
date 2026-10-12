package scas.power.sparse

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.boundary, boundary.break
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.ArrayPowerProduct

trait PowerProduct[N : {Numeric as numeric, ClassTag}] extends ArrayPowerProduct[N] {
  override def length = 0
  override def generator(n: Int) = {
    val z = ArrayBuffer.empty[(N, N)]
    z += ((numeric.fromInt(n), numeric.one))
    toArray(z)
  }
  override def gcd(x: Array[N], y: Array[N]) = {
    val z = ArrayBuffer.empty[(N, N)]
    for i <- 0 until x.size do {
      val j = y.indexOf(x(i))
      if j > -1 then {
        z += ((x(i), numeric.min(x.get(i), y.get(j))))
      }
    }
    toArray(z)
  }
  override def lcm(x: Array[N], y: Array[N]) = {
      val z = ArrayBuffer.empty[(N, N)]
      var i = 0
      var j = 0
      while i < x.size && j < y.size do {
        if x(i) < y(j) then {
          z += ((x(i), x.get(i)))
          i += 1
        } else if x(i) > y(j) then {
          z += ((y(j), y.get(j)))
          j += 1
        } else {
          z += ((x(i), numeric.max(x.get(i), y.get(j))))
          i += 1
          j += 1
        }
      }
      while i < x.size do {
        z += ((x(i), x.get(i)))
        i += 1
      }
      while j < y.size do {
        z += ((y(j), y.get(j)))
        j += 1
      }
      toArray(z)
  }
  extension (x: Array[N]) {
    override def multiply(y: Array[N]) = {
      val z = ArrayBuffer.empty[(N, N)]
      var i = 0
      var j = 0
      while i < x.size && j < y.size do {
        if x(i) < y(j) then {
          z += ((x(i), x.get(i)))
          i += 1
        } else if x(i) > y(j) then {
          z += ((y(j), y.get(j)))
          j += 1
        } else {
          z += ((x(i), x.get(i) + y.get(j)))
          i += 1
          j += 1
        }
      }
      while i < x.size do {
        z += ((x(i), x.get(i)))
        i += 1
      }
      while j < y.size do {
        z += ((y(j), y.get(j)))
        j += 1
      }
      toArray(z)
    }
    override def divide(y: Array[N]) = {
      val z = ArrayBuffer.empty[(N, N)]
      for i <- 0 until x.size do {
        val j = y.indexOf(x(i))
        val c = if j > -1 then y.get(j) else numeric.zero
        assert (x.get(i) >= c)
        if x.get(i) > c then {
          z += ((x(i), x.get(i) - c))
        }
      }
      toArray(z)
    }
    override def factorOf(y: Array[N]) = boundary {
      for i <- 0 until x.size do {
        val j = y.indexOf(x(i))
        val c = if j > -1 then y.get(j) else numeric.zero
        if x.get(i) > c then break(false)
      }
      true
    }
    override def projection(n: Int, m: Int) = {
      val z = ArrayBuffer.empty[(N, N)]
      for i <- 0 until x.size do {
        if numeric.toInt(x(i)) >= n && numeric.toInt(x(i)) < m then {
          z += ((x(i), x.get(i)))
        }
      }
      toArray(z)
    }
    override def convert(from: ArrayPowerProduct[N]): Array[N] = {
      val z = ArrayBuffer.empty[(N, N)]
      val index = from.variables.map(a => variables.indexOf(a))
      for i <- 0 until x.size do {
        val c = index(numeric.toInt(x(i)))
        assert (c > -1)
        z += ((numeric.fromInt(c), x.get(i)))
      }
      toArray(z.sortBy((s, _) => s))
    }
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
    override def size = x.length >> 1
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
    override def get(i: Int) = x(i + x.size)
    override def set(i: Int, c: N) = ???
  }
  def toArray(buffer: ArrayBuffer[(N, N)]) = (buffer.map(_._1) ++ buffer.map(_._2)).toArray
}
