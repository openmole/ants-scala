package ants

import ants.Ants.totalFood

import scala.util.Random

@main def model =
  implicit val rng: Random = new Random

  val time = System.currentTimeMillis()

  val res =
    for
      i <- 0 until 100
    yield
      val model = Ants(
        population = 50,
        diffusionRate = 0.1,
        evaporationRate = 0.1,
        worldWidth = 70,
        worldHeight = 70,
        foodSourceRadius = 5
      )
      val res = Ants.modelRun(model, 1000)
      totalFood(res)
      //

  println(res.sum / res.size)
  val end = System.currentTimeMillis()
  println(end - time)



@main def display =
  implicit val rng: Random = new Random

  val model = Ants(
    population = 125,
    diffusionRate = 0.5,
    evaporationRate = 1,
    worldWidth = 70,
    worldHeight = 70,
    foodSourceRadius = 5
  )

  Ants.modelConsoleDisplay(model, 1000)
