package computationalcognitivescience.fribble

import java.io.File

import com.github.tototoshi.csv._
import scala.collection.parallel.CollectionConverters._

object Simulation extends App {
  val nrRepresentations = 16
  val representationLength = 20
  val maxRounds = 6
  val featureLength = 4
  val featureDistortion = 0.2
  val featureOverlap = true
  val brainVoxelCount = 3
  val brainCompressionRatio = 10
  val brainNoiseRatio = 0.0
  val sampleSize = 10 // Samples per condition


  val initialAsymmetry = Set(0.25, 0.5, 0.75)
  val alignment = Set(0.0, 0.5, 1.0)
  val effortA = Set(0.5, 1.0)
  val effortB = Set(0.5, 1.0)
  val discernabilityThreshold = Set(0)

  def convergenceProbability(alignment: Double, jointEffort: Double): Double =
    (alignment + jointEffort) / 2.0

  val conditions = initialAsymmetry.flatMap(
    as =>
      effortA.flatMap(effA =>
        effortB.flatMap(effB =>
          discernabilityThreshold.flatMap(dt =>
            alignment.map(al =>
              Parameters(
                nrRepresentations,
                representationLength,
                featureLength,
                featureDistortion,
                featureOverlap,
                brainVoxelCount,
                brainCompressionRatio,
                brainNoiseRatio,
                as,
                al,
                effA,
                effB,
                dt,
                convergenceProbability(al, (effA + effB) / 2.0),
                maxRounds
              )))))).toVector

  val pairs = conditions.indices.flatMap(cid => List.tabulate(sampleSize)(id =>
    (cid * sampleSize + id -> (conditions(cid),
      Agent.pair(
        conditions(cid).nrRepresentations,
        conditions(cid).representationLength,
        conditions(cid).featureLength,
        conditions(cid).featureDistortion,
        conditions(cid).featureOverlap,
        brainVoxelCount,
        brainCompressionRatio,
        brainNoiseRatio,
        conditions(cid).initialAsymmetry,
        conditions(cid).effortA,
        conditions(cid).effortB,
        conditions(cid).discernabilityThreshold)
    )
      ))).toMap.par
  val interactions = pairs.map(pair => (pair._1, new Interaction(pair._2._2.a, pair._2._2.b, pair._2._1.convergenceProbability, pair._2._1.maxRounds)))
  val dialogs = interactions.mapValues(_.toList)

  val data = pairs.keys.map(k => k -> (pairs(k)._1, pairs(k)._2 :: dialogs(k))).seq.toMap


  // Write to files
  val dsf = new File("data/data_similarities_v6.csv")
  val dswriter = CSVWriter.open(dsf)

  dswriter.writeRow(Seq(
    "PairNr",
    "Fribble",
    "DisPRE",
    "DisPOST",
    "DisDIFF",
    "nrRepresentations",
    "representationLength",
    "asymmetry",
    "alignment",
    "effortA",
    "effortB",
    "discernabilityThreshold",
    "convergenceProbability",
    "maxRounds",
    "featureLength",
    "featureDistortion",
    "featureOverlap",
    "brainVoxelCount",
    "brainCompressionRatio",
    "brainNoiseRatio"
  ))

  for (pairNr <- data.keys.toList.sorted; fribble <- 0 until nrRepresentations) {
    val dialog = data(pairNr)._2
    val pars = data(pairNr)._1
    val pre = dialog.head
    val post = dialog.last
    val disPre = pre.a.representations(fribble).lDistance(pre.b.representations(fribble))
    val disPost = post.a.representations(fribble).lDistance(post.b.representations(fribble))

    val row = Seq(
      pairNr,
      fribble,
      disPre,
      disPost,
      disPost - disPre,
      pars.nrRepresentations,
      pars.representationLength,
      pars.initialAsymmetry,
      pars.alignment,
      pars.effortA,
      pars.effortB,
      pars.discernabilityThreshold,
      pars.convergenceProbability,
      pars.maxRounds,
      pars.featureLength,
      pars.featureDistortion,
      pars.featureOverlap,
      brainVoxelCount,
      brainCompressionRatio,
      brainNoiseRatio
    )

    dswriter.writeRow(row)
  }
  dswriter.close()

  val dff = new File("data/data_features_v6.csv")
  val dfwriter = CSVWriter.open(dff)

  val nrFeatures = data.head._2._2.head.a.representations.head.deriveFeatures.length
  val featureLabels = List.tabulate(nrFeatures)("F" + _).toSeq

  dfwriter.writeRow(
    Seq(
      "PairNr",
      "Agent",
      "Fribble"
    ) ++
      featureLabels.map(_ + "_PRE") ++
      featureLabels.map(_ + "_POST") ++
      Seq(
        "nrRepresentations",
        "representationLength",
        "asymmetry",
        "effortA",
        "effortB",
        "discernabilityThreshold",
        "convergenceProbability",
        "maxRounds",
        "featureLength",
        "featureDistortion",
        "featureOverlap",
        "brainVoxelCount",
        "brainCompressionRatio",
        "brainNoiseRatio"
      )
  )

  for (pairNr <- data.keys.toList.sorted; fribble <- 0 until nrRepresentations) {
    val dialog = data(pairNr)._2
    val pars = data(pairNr)._1

    val fPreA = dialog.head.a.representations.map(_.deriveFeatures)
    val fPreB = dialog.head.b.representations.map(_.deriveFeatures)
    val fPostA = dialog.last.a.representations.map(_.deriveFeatures)
    val fPostB = dialog.last.b.representations.map(_.deriveFeatures)

    val rowA =
      Seq(
        pairNr,
        "A",
        fribble
      ) ++
        fPreA(fribble).toSeq ++ fPostA(fribble).toSeq ++
        Seq(
          pars.nrRepresentations,
          pars.representationLength,
          pars.initialAsymmetry,
          pars.effortA,
          pars.effortB,
          pars.discernabilityThreshold,
          pars.convergenceProbability,
          pars.maxRounds,
          pars.featureLength,
          pars.featureDistortion,
          pars.featureOverlap,
          brainVoxelCount,
          brainCompressionRatio,
          brainNoiseRatio
        )

    val rowB =
      Seq(
        pairNr,
        "B",
        fribble
      ) ++
        fPreB(fribble).toSeq ++ fPostB(fribble).toSeq ++
        Seq(
          pars.nrRepresentations,
          pars.representationLength,
          pars.initialAsymmetry,
          pars.effortA,
          pars.effortB,
          pars.discernabilityThreshold,
          pars.convergenceProbability,
          pars.maxRounds,
          pars.featureLength,
          pars.featureDistortion,
          pars.featureOverlap,
          brainVoxelCount,
          brainCompressionRatio,
          brainNoiseRatio
        )

    dfwriter.writeRow(rowA)
    dfwriter.writeRow(rowB)
  }
  dfwriter.close()

  val dbf = new File("data/data_brain_v6.csv")
  val dbwriter = CSVWriter.open(dbf)

  val nrVoxels = data.head._2._2.head.a.representations.head.deriveSimulatedBrainData.length
  val voxelLabels = List.tabulate(nrVoxels)("V" + _).toSeq

  dbwriter.writeRow(
    Seq(
      "PairNr",
      "Agent",
      "Fribble"
    ) ++
      voxelLabels.map(_ + "_PRE") ++
      voxelLabels.map(_ + "_POST") ++
      Seq(
        "nrRepresentations",
        "representationLength",
        "asymmetry",
        "effortA",
        "effortB",
        "discernabilityThreshold",
        "convergenceProbability",
        "maxRounds",
        "featureLength",
        "featureDistortion",
        "featureOverlap",
        "brainVoxelCount",
        "brainCompressionRatio",
        "brainNoiseRatio"
      )
  )

  for (pairNr <- data.keys.toList.sorted; fribble <- 0 until nrRepresentations) {
    val dialog = data(pairNr)._2
    val pars = data(pairNr)._1

    val bPreA = dialog.head.a.representations(fribble).deriveSimulatedBrainData()
    val bPreB = dialog.head.b.representations(fribble).deriveSimulatedBrainData()
    val bPostA = dialog.last.a.representations(fribble).deriveSimulatedBrainData()
    val bPostB = dialog.last.b.representations(fribble).deriveSimulatedBrainData()

    val rowA =
      Seq(
        pairNr,
        "A",
        fribble
      ) ++
        bPreA(fribble).toSeq ++ bPostA(fribble).toSeq ++
        Seq(
          pars.nrRepresentations,
          pars.representationLength,
          pars.initialAsymmetry,
          pars.effortA,
          pars.effortB,
          pars.discernabilityThreshold,
          pars.convergenceProbability,
          pars.maxRounds,
          pars.featureLength,
          pars.featureDistortion,
          pars.featureOverlap,
          brainVoxelCount,
          brainCompressionRatio,
          brainNoiseRatio
        )

    val rowB =
      Seq(
        pairNr,
        "B",
        fribble
      ) ++
        bPreB(fribble).toSeq ++ bPostB(fribble).toSeq ++
        Seq(
          pars.nrRepresentations,
          pars.representationLength,
          pars.initialAsymmetry,
          pars.effortA,
          pars.effortB,
          pars.discernabilityThreshold,
          pars.convergenceProbability,
          pars.maxRounds,
          pars.featureLength,
          pars.featureDistortion,
          pars.featureOverlap,
          brainVoxelCount,
          brainCompressionRatio,
          brainNoiseRatio
        )

    dbwriter.writeRow(rowA)
    dbwriter.writeRow(rowB)
  }
  dbwriter.close()

}
