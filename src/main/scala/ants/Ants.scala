package ants

import scala.util.Random

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
  worldWidth: Int,
  worldHeight: Int,
  population: Int,
  diffusionRate: Double,
  evaporationRate: Double,
  nestLocation: (Double, Double)= (0.5,0.5),
  nestRadius: Double = 5.0,
  nestMaxScent: Double = 200.0,
  foodSourceLocations: Array[(Double, Double)] = Array((0.8, 0.5), (0.2,0.2), (0.1,0.9)),
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

  /**
   * food at ant's coordinates
   * @param ant
   * @return
   */
  def food(ant: Ant): Double =
    food(math.floor(ant.x).toInt)(math.floor(ant.y).toInt)

  def chemical(ant: Ant): Double =
    chemical(math.floor(ant.x).toInt)(math.floor(ant.y).toInt)

  def inNest(ant: Ant): Boolean =
    inNest(math.floor(ant.x).toInt)(math.floor(ant.y).toInt)

  def env(otherModel: Ants): Ants =
    this.chemical = otherModel.chemical
    this.food = otherModel.food
    this

  def totalFood: Double = food.flatten.sum

  def avgAntPosition: (Double, Double) =
    (this.ants.map(_.x).sum/this.ants.length.toDouble, this.ants.map(_.y).sum/this.ants.length.toDouble)




object Ants:

  /**
   *  Setup agents and patches
   */
  def setup(model: Ants)(implicit rng: Random): Ants =
    import model._

    val ants =
      Array.tabulate(population):i =>
        Ant(xc, yc, 360.0 * rng.nextDouble(), 0, 0.0)

    val inNest = Array.tabulate(worldWidth, worldHeight): (i,j) =>
      math.sqrt(math.pow(i.toDouble-xn,2.0) + math.pow(j.toDouble - yn, 2.0)) < nestRadius

    val nestScent = Array.tabulate(worldWidth, worldHeight):
      (i,j) => nestMaxScent - math.sqrt(math.pow(i.toDouble-xn,2.0) + math.pow(j.toDouble - yn, 2.0))

    val m = model.copy(ants = ants, nestScent=nestScent, inNest=inNest)

    // setup food sources: number not used besides display
    m.food =
      Array.tabulate(worldWidth, worldHeight): (i,j) =>
        def overlapWithSource(x: Double, y: Double) =
          math.sqrt(math.pow(i - x * worldWidth, 2.0) + math.pow(j - y * worldHeight, 2.0)) < foodSourceRadius

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
    diffuse(model.diffusionRate, model)
    evaporate(model.evaporationRate, model)

  def diffuse(diffusionRate: Double, model: Ants): Unit =
    // FIXME cloning is not necessary?
    val newVals = model.chemical.clone()
    val (height,width) = (model.chemical.length,model.chemical(0).length)

    for
      i <- model.chemical.indices
      j <- model.chemical(0).indices
    do
      val d = model.chemical(i)(j)
      if (!d.isNaN) {
        if (i >= 1) {
          newVals(i - 1)(j) = newVals(i - 1)(j) + (diffusionRate / 8) * d
        }
        if (i < height - 1) {
          newVals(i + 1)(j) = newVals(i + 1)(j) + (diffusionRate / 8) * d
        }
        if (j >= 1) {
          newVals(i)(j - 1) = newVals(i)(j - 1) + (diffusionRate / 8) * d
        }
        if (j < width - 1) {
          newVals(i)(j + 1) = newVals(i)(j + 1) + (diffusionRate / 8) * d
        }
        if (i >= 1 && j >= 1) {
          newVals(i - 1)(j - 1) = newVals(i - 1)(j - 1) + (diffusionRate / 8) * d
        }
        if (i >= 1 && j < width - 1) {
          newVals(i - 1)(j + 1) = newVals(i - 1)(j + 1) + (diffusionRate / 8) * d
        }
        if (i < height - 1 && j >= 1) {
          newVals(i + 1)(j - 1) = newVals(i + 1)(j - 1) + (diffusionRate / 8) * d
        }
        if (i < height - 1 && j < width - 1) {
          newVals(i + 1)(j + 1) = newVals(i + 1)(j + 1) + (diffusionRate / 8) * d
        }
        newVals(i)(j) = newVals(i)(j) - diffusionRate * d
      }

    model.chemical = newVals

  def evaporate(evaporationRate: Double, model: Ants): Unit =
    for
      i <- model.chemical.indices
      j <- model.chemical(0).indices
    do
      model.chemical(i)(j) = model.chemical(i)(j) * (1 - evaporationRate)

  def modelRun(model: Ants)(implicit rng: Random): Ants =
    val s0 = setup(model)
    Iterator.iterate(s0)(modelStep).take(100).toSeq.last




