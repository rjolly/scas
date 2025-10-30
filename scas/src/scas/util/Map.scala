package scas.util

import scala.jdk.CollectionConverters.MapHasAsScala

trait Map[K, V] extends java.util.SortedMap[K, V] {
  def asScala = MapHasAsScala(this).asScala
  def tailMapAsScala(key: K) = tailMap(key).asScala
}
