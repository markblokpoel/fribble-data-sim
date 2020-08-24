package computationalcognitivescience.fribble

import java.io.File

import com.github.tototoshi.csv._
import computationalcognitivescience.fribble.Implicits._

object Simulation extends App {
  val nrRepresentations = 16
  val representationLength = 15
  val maxRounds = 6
  val featureLength = 4
  val featureDistortion = 0.0
  val featureOverlap = true
  val sampleSize = 47

  val asymmetry = Seq(0.0, 0.25, 0.5, 0.75)
  val effortA = Seq(0.0, 0.2, 0.5, 0.9)
  val effortB = Seq(0.0, 0.2, 0.5, 0.9)
  val discernabilityThreshold = Seq(0)
  val convergenceProbability = Seq(0.1, 0.2, 0.3)

  val parameters = List.tabulate(sampleSize)(_ => Parameters(
    nrRepresentations,
    representationLength,
    featureLength,
    featureDistortion,
    featureOverlap,
    asymmetry.getRandom,
    effortA.getRandom,
    effortB.getRandom,
    discernabilityThreshold.getRandom,
    convergenceProbability.getRandom,
    maxRounds
  ))
  val pairs = parameters.map(pars => Agent.pair(pars.nrRepresentations, pars.representationLength, pars.featureLength, pars.featureDistortion, pars.featureOverlap, pars.asymmetry, pars.effortA, pars.effortB, pars.discernabilityThreshold))
  val interactions = pairs.map(pair => new Interaction(pair.a, pair.b, convergenceProbability.getRandom, maxRounds))
  val dialogs = interactions.map(_.toList)

  val data = parameters.indices.map(pairNr => {
    pairNr -> (parameters(pairNr), dialogs(pairNr))
  }).toMap

  //  println("\r=====PRE======")
  //  println("--Features A--")
  //  printFeatures(dialog.head.a.representations, dialog.head.a.representations.flatMap(r => r.deriveFeatures.zipWithIndex.map(di => (r, di._2) -> di._1)).toMap)
  //
  //  println("--Features B--")
  //  printFeatures(dialog.head.b.representations, dialog.head.b.representations.flatMap(r => r.deriveFeatures.zipWithIndex.map(di => (r, di._2) -> di._1)).toMap)
  //
  //  println("--Sim Matrix--")
  //  dialog.head.sim.print()
  //
  //  println("=====POST=====")
  //  println("--Features A--")
  //  printFeatures(dialog.last.a.representations, dialog.last.a.representations.flatMap(r => r.deriveFeatures.zipWithIndex.map(di => (r, di._2) -> di._1)).toMap)
  //
  //  println("--Features B--")
  //  printFeatures(dialog.last.b.representations, dialog.last.b.representations.flatMap(r => r.deriveFeatures.zipWithIndex.map(di => (r, di._2) -> di._1)).toMap)
  //
  //  println("--Sim Matrix--")
  //  dialog.last.sim.print()
  //
  //  def printFeatures(representations: Seq[Representation], scores: Map[(Representation, Int), Double]): Unit = {
  //    val features = scores.keys.map(_._2).toList.sorted
  //    val maxStrLen1 = representations.map(_.length).max
  //    val maxStrLen2 = math.max(10, features.map(_.toString.length).max)
  //
  //    println(
  //      " " * (maxStrLen1 + 1) + features
  //        .map(r2 => "F"+r2.toString + " " * (maxStrLen2 - r2.toString.length))
  //        .mkString)
  //    representations.foreach { r1 =>
  //      println(
  //        r1.toString + " " * (maxStrLen1 - r1.toString.length + 1) +
  //          features.toVector
  //            .map(r2 => f"${scores((r1, r2))}%2.2f" + " " * (maxStrLen2 - f"${scores((r1, r2))}%2.2f".length + 1))
  //            .mkString
  //      )
  //    }
  //  }

  // Write to file
  val dsf = new File("data/data_similarities.csv")
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

  val dff = new File("data/data_features.csv")
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
