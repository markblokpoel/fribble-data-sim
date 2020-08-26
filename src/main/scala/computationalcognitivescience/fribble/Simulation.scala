package computationalcognitivescience.fribble

import java.io.File

import com.github.tototoshi.csv._
import scala.collection.parallel.CollectionConverters._

object Simulation extends App {
  val nrRepresentations = 16
  val representationLength = 15
  val maxRounds = 6
  val featureLength = 4
  val featureDistortion = 0.0
  val featureOverlap = true
  val sampleSize = 10 // Samples per condition

  val asymmetry = Set(0.0, 0.25, 0.5, 0.75)
  val effortA = Set(0.0, 0.2, 0.5, 0.9)
  val effortB = Set(0.0, 0.2, 0.5, 0.9)
  val discernabilityThreshold = Set(0)
  val convergenceProbability = Set(0.1, 0.2, 0.3)

  val conditions = asymmetry.flatMap(as => effortA.flatMap(effA => effortB.flatMap(effB => discernabilityThreshold.flatMap(dt => convergenceProbability.map(cp =>
    Parameters(
      nrRepresentations,
      representationLength,
      featureLength,
      featureDistortion,
      featureOverlap,
      as,
      effA,
      effB,
      dt,
      cp,
      maxRounds
    )
  ))))).toVector

  val pairs = conditions.indices.flatMap(cid => List.tabulate(sampleSize)(id =>
    (cid*sampleSize + id -> (conditions(cid),
      Agent.pair(
        conditions(cid).nrRepresentations,
        conditions(cid).representationLength,
        conditions(cid).featureLength,
        conditions(cid).featureDistortion,
        conditions(cid).featureOverlap,
        conditions(cid).asymmetry,
        conditions(cid).effortA,
        conditions(cid).effortB,
        conditions(cid).discernabilityThreshold)
      )
    ))).toMap.par
  val interactions = pairs.map(pair => (pair._1, new Interaction(pair._2._2.a, pair._2._2.b, pair._2._1.convergenceProbability, pair._2._1.maxRounds)))
  val dialogs = interactions.mapValues(_.toList)

  val data = pairs.keys.map(k => k -> (pairs(k)._1, pairs(k)._2 :: dialogs(k))).seq.toMap

//    pairs.indices.map(pairNr => {
//    pairNr -> (pairs(pairNr)._1, pairs(pairNr)._2 :: dialogs(pairNr))
//  }).toMap

  // Write to files
  val dsf = new File("data/data_similarities_v3.csv")
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
    "effortA",
    "effortB",
    "discernabilityThreshold",
    "convergenceProbability",
    "maxRounds",
    "featureLength",
    "featureDistortion",
    "featureOverlap"
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
      pars.asymmetry,
      pars.effortA,
      pars.effortB,
      pars.discernabilityThreshold,
      pars.convergenceProbability,
      pars.maxRounds,
      pars.featureLength,
      pars.featureDistortion,
      pars.featureOverlap
    )

    dswriter.writeRow(row)
  }
  dswriter.close()

  val dff = new File("data/data_features_v3.csv")
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
        "featureOverlap"
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
          pars.asymmetry,
          pars.effortA,
          pars.effortB,
          pars.discernabilityThreshold,
          pars.convergenceProbability,
          pars.maxRounds,
          pars.featureLength,
          pars.featureDistortion,
          pars.featureOverlap
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
          pars.asymmetry,
          pars.effortA,
          pars.effortB,
          pars.discernabilityThreshold,
          pars.convergenceProbability,
          pars.maxRounds,
          pars.featureLength,
          pars.featureDistortion,
          pars.featureOverlap
        )

    dfwriter.writeRow(rowA)
    dfwriter.writeRow(rowB)
  }
  dfwriter.close()

}
