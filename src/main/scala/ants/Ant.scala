package ants

import scala.util.Random

case class Ant(
                x: Double,
                y: Double,
                angle: Double,
                departureTime: Int,
                foodCarried: Double
              ) {


  /**
   *  need to check that does not get out of the world?
   *   -> not necessary, wiggle checks this before fwd is called, but the procedure is unsafe
   * @param d
   * @return
   */
  def fwd(d: Double): Ant = this.copy(x=x+d*math.cos(angle),y=y+d*math.sin(angle))

  def canMove(d: Double)(implicit model: Ants): Boolean = {
    val (xh, yh) = (x+d*math.cos(angle),y+d*math.sin(angle))
    xh<model.worldWidth&&xh>=0.0&&yh<model.worldHeight&&yh>=0.0
  }

  // FIXME hardcoded wiggle parameters
  def wiggle(implicit rng: Random, model: Ants): Ant = {
    val newAngle = angle - 40.0 + 80.0*rng.nextDouble()
    this.copy(angle = (newAngle + (if (!canMove(1.0)) 180.0 else 0.0))% 360.0)
  }

  // FIXME hardcoded perception parameters -> may have interesting behavior on the model? optim? given food loc metaparam, calib? for any food config? -> rich model indeed
  def uphill(field: Array[Array[Double]])(implicit model: Ants): Ant = {
    val fieldAhead = fieldAtAngle(1.0, 0.0, field)
    val fieldRight = fieldAtAngle(1.0, 45.0, field)
    val fieldLeft = fieldAtAngle(1.0, -45.0, field)
    // in case of tie, minimise energy by not turning ; if same right and left: idem [not coded in netlogo: bias to the left in the model]
    this.copy(angle = angle + (if ((fieldAhead >= fieldRight && fieldAhead >= fieldLeft)||(fieldLeft==fieldRight)) 0.0 else if (fieldRight > fieldLeft) 45.0 else -45.0))
  }

  /**
   *
   *  rq : we check with an implicit world on worldWidth and worldHeight => wouldn't it be more generic to check on field size?
   * @param d
   * @param angle
   * @param field
   * @return
   */
  def fieldAtAngle(d: Double, beta: Double, field: Array[Array[Double]])(implicit model: Ants): Double = {
    val (xi, yi) = (math.floor(x + d*math.cos(angle + beta)).toInt, math.floor(y + d*math.sin(angle + beta)).toInt)
    if (xi < 0 || yi < 0 || xi >= model.worldWidth || yi >= model.worldHeight) 0.0
    else field(xi)(yi)
  }

}
