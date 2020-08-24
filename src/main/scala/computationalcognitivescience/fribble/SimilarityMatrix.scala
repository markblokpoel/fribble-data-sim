package computationalcognitivescience.fribble

case class SimilarityMatrix(m:  Map[(Int, Int), Int], representationsA: Seq[Representation], representationsB: Seq[Representation]) {
  require(representationsA.length == representationsB.length, "Dimensions of the similarity matrix should be equal: " +
    s".")
  require(m.keySet.forall( _._1 < representationsA.length))
  require(m.keySet.forall( _._2 < representationsB.length))

  def diagonal: List[Int] = (for(i <- representationsA.indices) yield m(i,i)).toList

  def print(): Unit = {
    val maxStrLen1 = representationsA.map(_.length).max
    val maxStrLen2 = math.max(10, representationsB.map(_.length).max)
    println(
      " " * (maxStrLen1 + 1) + representationsB
        .map(r2 => r2.toString + " " * (maxStrLen2 - r2.toString.length + 1))
        .mkString)
    representationsA.foreach { r1 =>
      println(
        r1.toString + " " * (maxStrLen1 - r1.toString.length + 1) +
          representationsB.toVector
            .map(r2 => s"${m((representationsA.indexOf(r1), representationsB.indexOf(r2)))}" + " " * (maxStrLen2 - f"${m((representationsA.indexOf(r1), representationsB.indexOf(r2)))}".length + 1))
            .mkString
      )
    }
  }

}
