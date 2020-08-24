package computationalcognitivescience.fribble

import scala.util.Random

case class Agent(representations: Seq[Representation],
                 effort: Double = 1.0,
                 discernabilityThreshold: Int = 0) {

  def transform(transformations: Map[Int, Map[Int, Option[Char]]]): Agent = {
    val transformedRepresentations =
      for (repId <- representations.indices) yield {
        val t = transformations(repId)
          .filter(_._2.isDefined)
          .view.mapValues(_.get)
          .toMap
        representations(repId).transform(t)
      }
    val discernedRepresentations =
      for(i <- transformedRepresentations.indices) yield {
        if(transformedRepresentations.forall(_.lDistance(transformedRepresentations(i)) >= discernabilityThreshold))
          // i-th transformed representation is sufficiently distinct from rest
          transformedRepresentations(i)
        else
          representations(i)
      }
    Agent(discernedRepresentations, effort)
  }


  def rsaMatrix(that: Agent): SimilarityMatrix = {
    require(this.representations.length == that.representations.length, "Cannot compare agents with different number of representations.")

    val sm =
      (for (i1 <- this.representations.indices; i2 <- that.representations.indices) yield {
        (i1, i2) -> this.representations(i1).lDistance(that.representations(i2))
      }).toMap

    SimilarityMatrix(sm, this.representations, that.representations)
  }
}

case object Agent {
  def randomBinary(nrRepresentations: Int,
                   strLength: Int,
                   effort: Double,
                   featureLength: Int = 3,
                   featureDistortion: Double = 0.0,
                   featureOverlap: Boolean = false,
                   discernabilityThreshold: Int = 0): Agent = {
    val representations = List.tabulate(nrRepresentations)(_ => Representation.random(strLength, Set('0', '1'), featureLength, featureDistortion, featureOverlap))
    Agent(representations, effort, discernabilityThreshold)
  }

  def pair(nrRepresentations: Int,
           strLength: Int,
           featureLength: Int,
           featureDistortion: Double,
           featureOverlap: Boolean,
           asymmetry: Double,
           effortA: Double,
           effortB: Double,
           discernabilityThreshold: Int): Pair = {
    val agentA = Agent.randomBinary(nrRepresentations, strLength, effortA, featureLength, featureDistortion, featureOverlap, discernabilityThreshold)
    val representationsA = agentA.representations
    val representationsB = representationsA.map(repr => {
      val content = repr.content.map(bit => {
        if (Random.nextDouble() <= asymmetry)
          if (bit == '0') '1' else '0'
        else bit
      })
      Representation(content, repr.featureLength, repr.featureDistortion, repr.featuresOverlap)
    })
    Pair(Agent(representationsA, effortA, discernabilityThreshold), Agent(representationsB, effortB, discernabilityThreshold))
  }
}