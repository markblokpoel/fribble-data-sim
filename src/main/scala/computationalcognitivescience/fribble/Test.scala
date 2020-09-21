package computationalcognitivescience.fribble

import Levenshtein._

object Test extends App {
  val r1 = Representation.random(150, Set('0','1'), 5, 0.0, featureOverlap = false, 10, 50, 0.0)
  val r2 = Representation.random(150, Set('0','1'), 5, 0.0, featureOverlap = false, 10, 50, 0.0)

  val t0 = System.currentTimeMillis()
  val res2 = levenshtein(r1.content.toList, r2.content.toList)
  val t1 = System.currentTimeMillis()

  println(res2 + " at +"+(t1-t0)+"ms")

  println(r1.deriveSimulatedBrainData.mkString("\n"))

}
