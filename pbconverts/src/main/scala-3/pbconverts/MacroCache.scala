package pbconverts

import scala.collection.mutable

object MacroCache {
  lazy val builderFunctionTrees: mutable.Map[String, mutable.Map[String, Any]] = mutable.Map.empty
}
