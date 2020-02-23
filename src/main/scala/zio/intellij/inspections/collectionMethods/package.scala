package zio.intellij.inspections

import org.jetbrains.plugins.scala.codeInspection.collections.likeCollectionClasses

package object collectionMethods {
  private[inspections] val `.map` = invocation("map").from(likeCollectionClasses)
}
