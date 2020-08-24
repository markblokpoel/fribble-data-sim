package computationalcognitivescience.fribble

import scala.util.Random

class Interaction(private var agentA: Agent,
                  private var agentB: Agent,
                  private val convergenceProbability: Double,
                  private val maxRounds: Int) extends Iterator[Pair] {

  var round = 1
  private val e = 0.1
  private val a = agentA.effort / (agentA.effort + agentB.effort + e)
  private val b = agentB.effort / (agentA.effort + agentB.effort + e)

  type RepId = Int
  type BitId = Int

  def transformations: Map[RepId, Map[BitId, Option[Char]]] = {
    (for(repId <- agentA.representations.indices) yield
      repId -> (for(bitId <- agentA.representations.head.content.indices) yield {
        val dart1 = Random.nextDouble()
        val dart2 = Random.nextDouble()
        if (dart1 <= convergenceProbability) {
          if (dart2 <= a) {
            // Towards A
            bitId -> Some(agentA.representations(repId).content(bitId))
          } else if (dart2 <= a + b) {
            // Towards B
            bitId -> Some(agentB.representations(repId).content(bitId))
          } else {
            // Towards new
            bitId -> Some(if (Random.nextBoolean()) '0' else '1')
          }
        } else {
          bitId -> None
        }
      }).toMap
    ).toMap
  }

  override def hasNext: Boolean = round <= maxRounds

  override def next(): Pair = {
    print(s"\rRound $round/$maxRounds")


    val curAgentA = agentA
    val curAgentB = agentB
    agentA = agentA.transform(transformations)
    agentB = agentB.transform(transformations)
    round += 1
    Pair(curAgentA, curAgentB)
  }
}
