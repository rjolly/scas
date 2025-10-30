package scas.util

import language.experimental.captureChecking
import scala.caps.Mutable

trait SortedMap[K, V] extends Map[K, V] with Mutable
