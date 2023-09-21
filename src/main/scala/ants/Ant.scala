package ants

import scala.util.Random

import java.lang.Math

case class Ant(
  x: Double,
  y: Double,
  angle: Double,
  departureTime: Int,
  foodCarried: Double)

object Ant:

  inline def angleModulo(inline a: Double) = (a % 360 + 360) % 360
  inline def setAngle(inline ant: Ant, inline angle: Double) =
    ant.copy(angle = angleModulo(angle))

  /**
   * need to check that does not get out of the world?
   * -> not necessary, wiggle checks this before fwd is called, but the procedure is unsafe
   *
   * @param d
   * @return
   */
  inline def fwd(ant: Ant, d: Double)(using Ants): Ant =
    if canMove(ant, d)
    then ant.copy(x = ant.x + d * Math.cos(Math.toRadians(ant.angle)), y = ant.y + d * Math.sin(Math.toRadians(ant.angle)))
    else ant


  inline def canMove(ant: Ant, d: Double)(using model: Ants): Boolean =
    val xh = ant.x + d * Math.cos(Math.toRadians(ant.angle))
    val yh = ant.y + d * Math.sin(Math.toRadians(ant.angle))
    Ants.insideTheWord(xh, yh, model)

  // FIXME hardcoded wiggle parameters
  def wiggle(ant: Ant)(implicit rng: Random, model: Ants): Ant =
    val newAngle = ant.angle + rng.nextInt(40) - rng.nextInt(40)
    val newAnt = setAngle(ant, newAngle)
    if !canMove(newAnt, 1.0)
    then setAngle(newAnt, newAngle + 180.0)
    else newAnt

  // FIXME hardcoded perception parameters -> may have interesting behavior on the model? optim? given food loc metaparam, calib? for any food config? -> rich model indeed
  def uphill(ant: Ant, field: Array[Array[Double]])(implicit model: Ants): Ant =

    // in case of tie, minimise energy by not turning ; if same right and left: idem [not coded in netlogo: bias to the left in the model]
    //def direction = Array(0.0, -45.0, 45.0).maxBy(d => fieldAtAngle(ant, 1.0, d, field))

    val ahead = fieldAtAngle(ant, 1.0, 0.0, field)
    val left = fieldAtAngle(ant, 1.0, -45.0, field)
    val right = fieldAtAngle(ant, 1.0, 45.0, field)

    if (right > ahead) || (left > ahead)
    then
      if right > left
      then setAngle(ant, angle = ant.angle + 45)
      else setAngle(ant, angle = ant.angle - 45)
    else ant

  /**
   *
   * rq : we check with an implicit world on worldWidth and worldHeight => wouldn't it be more generic to check on field size?
   *
   * @param d
   * @param angle
   * @param field
   * @return
   */
  def fieldAtAngle(ant: Ant, d: Double, beta: Double, field: Array[Array[Double]])(implicit model: Ants): Double =
    val am = Math.toRadians(angleModulo(ant.angle + beta))
    val (xi, yi) = (Math.floor(ant.x + d * Math.cos(am)).toInt, Math.floor(ant.y + d * Math.sin(am)).toInt)
    if Ants.insideTheWord(xi, yi, model)
    then field(xi)(yi)
    else 0.0


  def antActions(ant: Ant, model: Ants)(implicit rng: Random): Ant =
    import model._

    given Ants = model

    if time < ant.departureTime
    then ant
    else
      val newAnt =
        if ant.foodCarried == 0.0
        then
          // look for food
          if Ants.food(model, ant.x, ant.y) > 0
          then
            // pick up food: modify food state var
            val (xi, yi) = (Math.floor(ant.x).toInt, Math.floor(ant.y).toInt)
            val f = model.food(xi)(yi)
            model.food(xi)(yi) = f.copy(_2 = f._2 - 1)
            setAngle(ant.copy(foodCarried = ant.foodCarried + 1.0), ant.angle + 180.0)
          else
            // uphill chemical
            val c = Ants.chemical(model, ant.x, ant.y)
            if c >= 0.05 && c < 2 // FIXME hardcoded parameters
            then uphill(ant, model.chemical)
            else ant
        else
          // return to nest
          if Ants.inNest(model, ant)
          then
            // drop food and make a turn
            setAngle(ant.copy(foodCarried = 0.0), ant.angle + 180.0)
          else
            // drop chemical and head towards nest
            val (xi, yi) = (Math.floor(ant.x).toInt, math.floor(ant.y).toInt)
            model.chemical(xi)(yi) = model.chemical(xi)(yi) + chemicalDropUnit
            uphill(ant, model.nestScent)

      fwd(
        wiggle(newAnt),
        1.0
      )



