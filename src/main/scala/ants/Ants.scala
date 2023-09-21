package ants

import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.lang.Math
import scala.collection.mutable



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
  diffusionRate: Double,
  evaporationRate: Double,
  worldWidth: Int,
  worldHeight: Int,
  foodSourceLocations: Array[(Double, Double)],
  chemicalDropUnit: Double,
  time: Int,
  ants: Seq[Ant],
  nestScent: Array[Array[Double]],
  inNest: Array[Array[Boolean]],
  neighborhoodCache: Array[Array[Array[Array[Int]]]],
  //  antChemicalMin: Double,
  //  antChemicalMax: Double,
  var chemical: Array[Array[Double]],
  var food: Array[Array[(Int, Int)]])


object Ants:

  def apply(
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
   chemicalDropUnit: Double = 60.0)(using rng: Random) =

    val xc: Double = worldWidth.toDouble / 2.0
    val yc: Double = worldHeight.toDouble / 2.0

    val ants =
      Array.tabulate(population): i =>
        Ant(xc, yc, rng.nextInt(360), departureTime = i, 0.0)

    val xn: Double = nestLocation._1
    val yn: Double = nestLocation._2

    val inNest = Array.tabulate(worldWidth, worldHeight): (i, j) =>
      Math.sqrt(Math.pow(i.toDouble - xn * worldWidth, 2.0) + Math.pow(j.toDouble - yn * worldHeight, 2.0)) < nestRadius

    val nestScent =
      Array.tabulate(worldWidth, worldHeight): (i, j) =>
        nestMaxScent - Math.sqrt(Math.pow(i.toDouble - xn * worldWidth, 2.0) + Math.pow(j.toDouble - yn * worldHeight, 2.0))

    //val m = model.copy(ants = ants, nestScent = nestScent, inNest = inNest)

    // setup food sources: number not used besides display
    val food =
      Array.tabulate(worldWidth, worldHeight): (i, j) =>
        def overlapWithSource(x: Double, y: Double) =
          Math.sqrt(Math.pow(i - x * worldWidth, 2.0) + Math.pow(j - y * worldHeight, 2.0)) < foodSourceRadius

        val source = foodSourceLocations.indexWhere(overlapWithSource)
        if  source > -1
        then source -> (1 + rng.nextInt(2))
        else -1 -> 0

    //println(food.map(_.mkString(" ")).mkString("\n"))

    // color: no display

    // 0 chemicals at setup
    val chemical = Array.fill(worldWidth, worldHeight)(0.0)

    val neighborhoodCache =
      Array.tabulate(worldWidth, worldHeight): (i, j) =>
        Ants.neighbors(i, j, worldWidth, worldHeight).map((x, y) => Array(x, y))

    new Ants(
      diffusionRate = diffusionRate,
      evaporationRate = evaporationRate,
      worldWidth = worldWidth,
      worldHeight = worldHeight,
      foodSourceLocations = foodSourceLocations,
      chemicalDropUnit = chemicalDropUnit,
      time = 0,
      ants = ants,
      nestScent = nestScent,
      inNest = inNest,
      neighborhoodCache = neighborhoodCache,
      chemical = chemical,
      food = food)


  /**
   * food at ant's coordinates
   *
   * @param ant
   * @return
   */
  def food(ants: Ants, x: Double, y: Double): Int =
    ants.food(Math.floor(x).toInt)(Math.floor(y).toInt)._2

  def chemical(ants: Ants, x: Double, y: Double): Double =
    ants.chemical(Math.floor(x).toInt)(Math.floor(y).toInt)

  def inNest(ants: Ants, ant: Ant): Boolean =
    ants.inNest(Math.floor(ant.x).toInt)(Math.floor(ant.y).toInt)

  def totalFood(ants: Ants): Int = ants.food.flatten.map(_._2).sum

  def foodBySource(ants: Ants) =
    for
      i <- 0 until ants.foodSourceLocations.length
    yield
      ants.food.flatten.filter((s, _) => s == i).map((_, q) => q).sum

  def avgAntPosition(ants: Ants): (Double, Double) =
    (ants.ants.map(_.x).sum / ants.ants.length.toDouble, ants.ants.map(_.y).sum / ants.ants.length.toDouble)



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
    val nextAnts = model.ants.map(Ant.antActions(_, model))
    val nextStep = model.copy(ants = nextAnts)
    chemicals(nextStep)
    nextStep.copy(time = nextStep.time + 1)

  /**
   * Diffuse and evaporate chemicals
   * @param ants
   */
  def chemicals(model: Ants): Unit =
    diffuse(model)
    evaporate(model)

  def neighbors(x: Int, y: Int, width: Int, height: Int) =
    Array(
      (x - 1, y - 1),
      (x - 1, y),
      (x - 1, y + 1),
      (x, y - 1),
      (x, y + 1),
      (x + 1, y -1),
      (x + 1, y),
      (x + 1, y + 1)
    ).filter((x, y) => insideTheWord(x, y, width, height))
//    val buffer = mutable.ArrayBuffer[(Int, Int)]()
//    for
//      ox <- -neighborhoodSize to neighborhoodSize
//      oy <- -neighborhoodSize to neighborhoodSize
//      if ox != 0 || oy != 0
//      nx = x + ox
//      ny = y + oy
//      if insideTheWord(nx, ny, width, height)
//    do buffer += ((nx, ny))
//    buffer.toArray

  def diffuse(model: Ants): Unit =
    val newVals = Array.ofDim[Double](model.worldWidth, model.worldHeight)

    loop(0, _ < model.worldWidth, _ + 1): i =>
      loop(0, _ < model.worldHeight, _ + 1): j =>
        val d = model.chemical(i)(j)

        var totalN = 0.0
        val allN = model.neighborhoodCache(i)(j)
        loop(0, _ < allN.length, _ + 1): n =>
          val curN = allN(n)
          val nx = curN(0)
          val ny = curN(1)
          totalN += model.chemical(nx)(ny)

        // See https://github.com/NetLogo/NetLogo/blob/c02bff554418e513165332d095958b0c8e3f0fc7/netlogo-core/src/main/agent/Topology.scala#L159
        newVals(i)(j) = d + model.diffusionRate * (totalN / allN.length - d)

    model.chemical = newVals

  inline def insideTheWord(i: Double, j: Double, width: Int, height: Int): Boolean = i >= 0 && j >= 0 && i < width && j < height
  inline def insideTheWord(i: Double, j: Double, model: Ants): Boolean = insideTheWord(i, j, model.worldWidth, model.worldHeight)

  def evaporate(model: Ants): Unit =
    loop(0, _ < model.worldWidth, _ + 1): i =>
      loop(0, _ < model.worldHeight, _ + 1): j =>
        val chem = model.chemical(i)(j)
        model.chemical(i)(j) = chem * (1 - model.evaporationRate)

  def modelRun(model: Ants, step: Int)(implicit rng: Random): Ants =
    Iterator.iterate(model)(modelStep).drop(step - 1).next()

  def modelStates(model: Ants, step: Int)(implicit rng: Random) =
    Iterator.iterate(model)(modelStep).take(step)


  def modelConsoleDisplay(model: Ants, step: Int)(implicit rng: Random): Unit =
    Iterator.iterate(model)(modelStep).take(step).foreach: m =>

      val buffer = display(m)
      import com.github.tomaslanger.chalk.Ansi
      //print(Ansi.eraseLine())
      print(Ansi.eraseScreen())
      //print(Ansi.cursorLeft(model.worldWidth))

      println(buffer)
      Thread.sleep(10)


  def display(model: Ants) =
    val buffer =
      Array.tabulate[Char](model.worldWidth, model.worldHeight): (i, j) =>
        val antsPosition = model.ants.map(a => (Math.floor(a.x).toInt, Math.floor(a.y).toInt)).toSet
        if antsPosition.contains((i, j))
        then 'a'
        else
          if model.inNest(i)(j)
          then 'N'
          else
            if model.food(i)(j)._2 > 0
            then model.food(i)(j).toString.charAt(0)
            else
              if model.chemical(i)(j) > 0.02
              then 'P'
              else ' '

    buffer.map(_.mkString).mkString("\n")

  /** standard C-style for loop */
  inline def loop[A](
    inline start: A,
    inline condition: A => Boolean,
    inline advance: A => A)(inline loopBody: A => Any): Unit =
    var a = start
    while condition(a) do
      loopBody(a)
      a = advance(a)



