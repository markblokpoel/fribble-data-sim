package computationalcognitivescience.fribble

case class Pair(a: Agent, b: Agent) {
  def sim: SimilarityMatrix = a.rsaMatrix(b)
}
