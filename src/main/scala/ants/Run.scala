package ants

import scala.util.Random

@main def model =
  implicit val rng: Random = new Random




  val time = System.currentTimeMillis()


  val res =
    for
      i <- 0 until 100
    do
      val model = Ants(
        population = 125,
        diffusionRate = 0.5,
        evaporationRate = 0.2,
        //    worldWidth = 30,
        //    worldHeight = 30,
        //    foodSourceRadius = 2
      )
      Ants.modelRun(model, 1000)
  val end = System.currentTimeMillis()
  println(end - time)