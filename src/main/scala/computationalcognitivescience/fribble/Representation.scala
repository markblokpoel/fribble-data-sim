package computationalcognitivescience.fribble

import scala.util.Random

case class Representation(content: String,
                          featureLength: Int = 3,
                          featureDistortion: Double = 0.0,
                          featuresOverlap: Boolean = false,
                          brainVoxelCount: Int = 10,
                          brainCompressionRatio: Int = 50,
                          brainNoiseRatio: Double = 0.0) {
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

  def deriveSimulatedBrainData: List[Double] = {
    val skipTotal = math.ceil(math.log(brainCompressionRatio) / math.log(2)).toInt
    val voxelSize = (content.length - skipTotal) / brainVoxelCount
    val skipSize = skipTotal / brainVoxelCount

    def groupBy(value: String): List[String] = {
      if(value.length<voxelSize) List[String]()
      else {
        val (voxel, _rest) = value.splitAt(voxelSize)
        val (_, rest) = _rest.splitAt(skipSize)
        groupBy(rest) ++ List(voxel.mkString)
      }
    }
    val voxels = groupBy(content)
      .map(bitString => {
        bitString.map(bit => {
          if(Random.nextDouble() >= brainNoiseRatio) if(bit == '0') '1' else '0' // flip bit for distortion
          else bit
        })
      })

    val maxVoxelValue = (0 until voxelSize).map(math.pow(2,_)).sum
    val simulatedBrainData: List[Double] = voxels.map(Integer.parseInt(_, 2) / maxVoxelValue)
    simulatedBrainData
  }

  def lDistance(that: Representation): Int =
    Levenshtein.levenshtein(this.content.toList, that.content.toList)
}

case object Representation {
  def random(length: Int,
             alphabet: Set[Char],
             featureLength: Int = 3,
             featureDistortion: Double = 0.0,
             featureOverlap: Boolean = false,
             brainVoxelCount: Int,
             brainCompressionRatio: Int,
             brainNoiseRatio: Double): Representation = Representation(
    List.tabulate(length)(_ => alphabet.toList(Random.nextInt(alphabet.size))).mkString,
    featureLength,
    featureDistortion,
    featureOverlap,
    brainVoxelCount,
    brainCompressionRatio,
    brainNoiseRatio
  )

  def randomTransformation(stringLength: Int, probability: Double, alphabet: Set[Char]): Map[Int, Char] = {
    (for(i <- 0 until stringLength) yield
      if(Random.nextDouble() <= probability) i -> alphabet.toList(Random.nextInt(alphabet.size))
      else -1 -> 'a')
      .filter(_._1 >= 0)
      .toMap
  }
}