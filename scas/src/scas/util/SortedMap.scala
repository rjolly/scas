package scas.util

import language.experimental.captureChecking
import scala.caps.Mutable

trait SortedMap[K, V] extends java.util.SortedMap[K, V] with Mutable
