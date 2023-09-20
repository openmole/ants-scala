package ants

import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.lang.Math

/**
 * Model, including model state (time and ants; world state is stored as var arrays for performance)
 *
 * @param worldWidth
 * @param worldHeight
 * @param population
 * @param diffusionRate
 * @param evaporationRate
 * @param nestLocation
 * @param nestRadius
 * @param nestMaxScent
 * @param foodSourceLocations
 * @param foodSourceRadius
 * @param time
 * @param ants
 */
case class Ants(
  population: Int,
  diffusionRate: Double,
  evaporationRate: Double,
  worldWidth: Int = 70,
  worldHeight: Int = 70,
  nestLocation: (Double, Double) = (0.5, 0.5),
  nestRadius: Double = 5.0,
  nestMaxScent: Double = 200.0,
  foodSourceLocations: Array[(Double, Double)] = Array((0.8, 0.5), (0.2, 0.2), (0.1, 0.9)),
  foodSourceRadius: Double = 5.0,
  chemicalDropUnit: Double = 60.0,
  time: Int = 0,
  ants: Seq[Ant] = Seq.empty[Ant],
  nestScent: Array[Array[Double]] = Array.empty[Array[Double]],
  inNest: Array[Array[Boolean]] = Array.empty[Array[Boolean]]):

  var chemical: Array[Array[Double]] = Array.empty[Array[Double]]
  var food: Array[Array[Int]] = Array.empty[Array[Int]]

  def xc: Double = worldWidth.toDouble / 2.0
  def yc: Double = worldHeight.toDouble / 2.0

  def xn: Double = nestLocation._1
  def yn: Double = nestLocation._2


  def inNest(ant: Ant): Boolean =
    inNest(Math.floor(ant.x).toInt)(Math.floor(ant.y).toInt)

  def env(otherModel: Ants): Ants =
    this.chemical = otherModel.chemical
    this.food = otherModel.food
    this

  def totalFood: Double = food.flatten.sum

  def avgAntPosition: (Double, Double) =
    (this.ants.map(_.x).sum/this.ants.length.toDouble, this.ants.map(_.y).sum/this.ants.length.toDouble)




object Ants:

  /**
   * food at ant's coordinates
   *
   * @param ant
   * @return
   */
  def food(ants: Ants, x: Double, y: Double): Double =
    ants.food(Math.floor(x).toInt)(Math.floor(y).toInt)

  def chemical(ants: Ants, x: Double, y: Double): Double =
    ants.chemical(Math.floor(x).toInt)(Math.floor(y).toInt)


  /**
   *  Setup agents and patches
   */
  def setup(model: Ants)(implicit rng: Random): Ants =
    import model._

    val ants =
      Array.tabulate(population):i =>
        Ant(xc, yc, 360.0 * rng.nextDouble(), departureTime = i, 0.0)

    val inNest = Array.tabulate(worldWidth, worldHeight): (i,j) =>
      Math.sqrt(Math.pow(i.toDouble - xn * worldWidth, 2.0) + Math.pow(j.toDouble - yn * worldHeight, 2.0)) < nestRadius

    val nestScent =
      Array.tabulate(worldWidth, worldHeight): (i,j) =>
        nestMaxScent - Math.sqrt(Math.pow(i.toDouble - xn * worldWidth,2.0) + Math.pow(j.toDouble - yn * worldHeight, 2.0))

    val m = model.copy(ants = ants, nestScent = nestScent, inNest = inNest)

    // setup food sources: number not used besides display
    m.food =
      Array.tabulate(worldWidth, worldHeight): (i,j) =>
        def overlapWithSource(x: Double, y: Double) =
          Math.sqrt(Math.pow(i - x * worldWidth, 2.0) + Math.pow(j - y * worldHeight, 2.0)) < foodSourceRadius

        if foodSourceLocations.exists(overlapWithSource)
        then 1 + rng.nextInt(2)
        else 0

    // color: no display

    // 0 chemicals at setup
    m.chemical = Array.fill(worldWidth, worldHeight)(0.0)

    m

  /**
   * Model step:
   *  - move ants
   *  - diffuse chem
   *  - evaporate chemical
   *
   * @param model
   * @return
   */
  def modelStep(model: Ants)(implicit rng: Random): Ants =
    println(s"=====Step ${model.time}=====\nTotal food = ${model.totalFood}\nAvg pos = ${model.avgAntPosition}")
    val nextAnts = model.ants.map(Ant.antActions(_, model))
    val nextStep = model.copy(ants = nextAnts).env(model)
    chemicals(nextStep)
    nextStep.copy(time = nextStep.time + 1).env(nextStep)

  /**
   * Diffuse and evaporate chemicals
   * @param ants
   */
  def chemicals(model: Ants): Unit =
    diffuse(model)
    evaporate(model)

  def diffuse(model: Ants): Unit =
    def neighbors(x: Int, y: Int, neighborhoodSize: Int = 1) =
      for
        ox <- -neighborhoodSize to neighborhoodSize
        oy <- -neighborhoodSize to neighborhoodSize
        if ox != oy
        nx = x + ox
        ny = y + oy
        if insideTheWord(nx, ny, model)
      yield (nx, ny)

    val newVals = Array.fill(model.worldWidth, model.worldHeight)(0.0)

    for
      i <- 0 until model.worldWidth
      j <- 0 until model.worldHeight
    do
      val d = model.chemical(i)(j)
      val allN = neighbors(i, j)
      val diffused = (d * model.diffusionRate) / allN.length
      for
        (nx, ny) <- allN
      do
        newVals(nx)(ny) = newVals(nx)(ny) + diffused
      newVals(i)(j) = d - (d * model.diffusionRate)

    model.chemical = newVals

  inline def insideTheWord(i: Double, j: Double, model: Ants) = i >= 0 && j >= 0 && i < model.worldWidth && j < model.worldHeight

  def evaporate(model: Ants): Unit =
    for
      i <- 0 until model.worldWidth
      j <- 0 until model.worldHeight
    do
      model.chemical(i)(j) = model.chemical(i)(j) * (1 - model.evaporationRate)

  def modelRun(model: Ants, step: Int)(implicit rng: Random): Ants =
    val s0 = setup(model)
    Iterator.iterate(s0)(modelStep).drop(step - 1).next()




