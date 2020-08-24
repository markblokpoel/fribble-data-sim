package computationalcognitivescience.fribble

import scala.util.Random

case class Representation(content: String,
                          featureLength: Int = 3,
                          featureDistortion: Double = 0.0,
                          featuresOverlap: Boolean = false) {
  def head: Char = content.head
  def tail: Representation = Representation(content.tail)
  def isEmpty: Boolean = content.isEmpty
  def length: Int = content.length

  def transform(replacements: Map[Int, Char]): Representation = {
    val c = content.indices
      .map(i => if(replacements.isDefinedAt(i)) replacements(i) else content(i))
      .mkString
    Representation(c, featureLength, featureDistortion, featuresOverlap)
  }

  override def toString: String = content

  /*
  Assuming binary strings for now..
   */
  def deriveFeatures: List[Double] = {
    def groupBy(values: Seq[String]): Seq[String] = {
      if(values.length<featureLength) Seq[String]()
      else {
        val (first, tail) = values.splitAt(featureLength)
        if(featuresOverlap)
          groupBy(first.splitAt(first.length/2)._2 ++ tail) ++ Seq(first.mkString)
        else
          groupBy(tail) ++ Seq(first.mkString)
      }
    }

    val seq = groupBy(content.map(_.toString)) // split into base groups
      .map(bitString => {
        bitString.map(bit => {
          if(Random.nextDouble() >= featureDistortion) if(bit == '0') '1' else '0' // flip bit for distortion
          else bit
        })
      })
      .map(Integer.parseInt(_, 2)) // convert bit string to integer
      .toList
    val max = Integer.parseInt(List.tabulate(featureLength)(_ => '1').mkString, 2)
    seq.map(_.doubleValue / max)
  }

  private def min(a: Int, b: Int, c: Int): Int = math.min(math.min(a, b), c)

  def lDistance(that: Representation): Int = {
    if(this.isEmpty || that.isEmpty)
      0
    else if(this.head == that.head)
      this.tail.lDistance(that.tail)
    else {
      1 + min(
        this.lDistance(that.tail),
        this.tail.lDistance(that),
        this.tail.lDistance(that.tail)
      )
    }
  }
}

case object Representation {
  def random(length: Int,
             alphabet: Set[Char],
             featureLength: Int = 3,
             featureDistortion: Double = 0.0,
             featureOverlap: Boolean = false): Representation = Representation(
    List.tabulate(length)(_ => alphabet.toList(Random.nextInt(alphabet.size))).mkString,
    featureLength,
    featureDistortion,
    featureOverlap
  )

  def randomTransformation(stringLength: Int, probability: Double, alphabet: Set[Char]): Map[Int, Char] = {
    (for(i <- 0 until stringLength) yield
      if(Random.nextDouble() <= probability) i -> alphabet.toList(Random.nextInt(alphabet.size))
      else -1 -> 'a')
      .filter(_._1 >= 0)
      .toMap
  }
}