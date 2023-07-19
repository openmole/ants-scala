package ants

import scala.util.Random

object Run extends App {

  implicit val rng: Random = new Random

  val model = Ants(
    worldWidth = 100,
    worldHeight = 100,
    population = 100,
    diffusionRate = 0.5,
    evaporationRate = 0.2
  )

  val res = Ants.modelRun(model)

}
