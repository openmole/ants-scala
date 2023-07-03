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
               foodSourceLocations: Array[(Double,Double)] = Array((0.8,0.5),(0.2,0.2),(0.1,0.9)),
               foodSourceRadius: Double = 5.0,
               chemicalDropUnit: Double = 60.0,
               time: Int = 0,
               ants: Seq[Ant] = Seq.empty[Ant],
               nestScent: Array[Array[Double]] = Array.empty[Array[Double]],
               inNest: Array[Array[Boolean]] = Array.empty[Array[Boolean]]
               ) {
  var chemical: Array[Array[Double]] = Array.empty[Array[Double]]
  var food: Array[Array[Double]] = Array.empty[Array[Double]]

  def xc: Double = worldWidth.toDouble / 2.0
  def yc: Double = worldHeight.toDouble / 2.0

  def xn: Double = nestLocation._1
  def yn: Double = nestLocation._2

  /**
   * food at ant's coordinates
   * @param ant
   * @return
   */
  def food(ant: Ant): Double = food(math.floor(ant.x).toInt)(math.floor(ant.y).toInt)

  def chemical(ant: Ant): Double = chemical(math.floor(ant.x).toInt)(math.floor(ant.y).toInt)

  def inNest(ant: Ant): Boolean = inNest(math.floor(ant.x).toInt)(math.floor(ant.y).toInt)
  
}


object Ants {

  /**
   *  Setup agents and patches
   */
  def setup(model: Ants)(implicit rng: Random): Ants = {
    import model._

    val ants = Array.tabulate(population)(i => Ant(xc,yc, 360.0*rng.nextDouble(), i, 0.0))
    val inNest = Array.tabulate(worldWidth, worldHeight){case (i,j) => math.sqrt(math.pow(i.toDouble-xn,2.0) + math.pow(j.toDouble - yn, 2.0)) < nestRadius }
    val nestScent = Array.tabulate(worldWidth, worldHeight){case (i,j) => nestMaxScent - math.sqrt(math.pow(i.toDouble-xn,2.0) + math.pow(j.toDouble - yn, 2.0))}
    val m = model.copy(ants = ants, nestScent=nestScent, inNest=inNest)

    // setup food sources: number not used besides display
    m.food = Array.tabulate(worldWidth, worldHeight){case (i,j) => if (foodSourceLocations.exists{case (xs,ys)=> math.sqrt(math.pow(i.toDouble-xs,2.0) + math.pow(j.toDouble - ys, 2.0)) < foodSourceRadius}) 1.0+rng.nextInt(2).toDouble else 0.0 }

    // color: no display

    // 0 chemicals at setup
    m.chemical = Array.fill(worldWidth, worldHeight)(0.0)

    m
  }

  /**
   * Model step:
   *  - move ants
   *  - diffuse chem
   *  - evaporate chemical
   *
   * @param model
   * @return
   */
  def modelStep(model: Ants): Ants = {
    
    val nextAnts = model.ants.map(Ant.antActions(_, model))
    

    val nextStep = model.copy(ants = nextAnts)
    chemicals(nextStep)
    nextStep
  }

  /**
   * Diffuse and evaporate chemicals
   * @param ants
   */
  def chemicals(model: Ants): Unit = {

  }



}

