package pbconverts

import scala.collection.mutable

object MacroCache {
  private var builderCount = 0
  private var identityCount = 0

  def getBuilderId: Int = builderCount.synchronized { builderCount += 1; builderCount }
  def getIdentityId: Int = identityCount.synchronized { identityCount += 1; identityCount }

  lazy val builderFunctionTrees: mutable.Map[Int, mutable.Map[String, Any]] = mutable.Map.empty
}
