package ants

import scala.util.Random

@main def model =
  implicit val rng: Random = new Random

  val model = Ants(
    population = 125,
    diffusionRate = 0.5,
    evaporationRate = 0.2
  )

  val res = Ants.modelRun(model, 1000)
