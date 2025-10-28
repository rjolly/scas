package scas.util

import scala.math.Ordering

class TreeMap[K, V](m: java.util.SortedMap[K, V]) extends java.util.TreeMap[K, V](m) with SortedMap[K, V] {
  def this(ord: Ordering[K]) = this(new java.util.TreeMap(ord))
}
