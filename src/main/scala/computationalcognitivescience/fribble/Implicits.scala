package computationalcognitivescience.fribble

import scala.util.Random

object Implicits {
  implicit class SeqRandomizer[A](seq: Seq[A]) {
    def getRandom: A = seq(Random.nextInt (seq.length))
  }
}
