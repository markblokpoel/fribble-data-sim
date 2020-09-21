package computationalcognitivescience.fribble

case class Parameters(nrRepresentations: Int,
                      representationLength: Int,
                      featureLength: Int,
                      featureDistortion: Double,
                      featureOverlap: Boolean,
                      brainVoxelCount: Int,
                      brainCompressionRatio: Int ,
                      brainNoiseRatio: Double ,
                      initialAsymmetry: Double,
                      alignment: Double,
                      effortA: Double,
                      effortB: Double,
                      discernabilityThreshold: Int,
                      convergenceProbability: Double,
                      maxRounds: Int)
