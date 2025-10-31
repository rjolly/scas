package scas.util

import language.experimental.captureChecking
import scala.caps.Mutable

trait SortedMap[K, V] extends Map[K, V] with Mutable {
  update def _put(key: K, value: V) = put(key, value)
  update def _remove(key: K) = remove(key)
}
