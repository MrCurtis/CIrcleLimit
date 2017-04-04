package circle_limit

import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.DynamicImplicits.number2dynamic
import org.scalajs.dom.html
import spire.math.Complex
import spire.implicits._
import circle_limit.maths.CircleImplicits._

import circle_limit.maths.{
  Arc,
  Geodesic,
  Group,
  Line,
  MoebiusTransformation,
  SpaceType
}
import circle_limit.maths.Imaginary.i
import circle_limit.graphics.{Converter, Box}

@JSExport
object CircleLimitApp {
  @JSExport
  def main(movie: html.Div): Unit = {

    val displayWidth = 880
    val displayHeight = 880
    val converter = Converter(
      Box(-1.0, -1.0, 2.0, 2.0),
      Box(0.0, 0.0, displayWidth, displayHeight)
    )
    val transformArc = converter.convertArcToSvg _

    val document = js.Dynamic.global.document
    val playground = document.getElementById("movie")

    var s = js.Dynamic.global.Snap(displayWidth, displayHeight)
    var bigCircle = s.circle(displayWidth/2, displayHeight/2, 400).attr(
      js.Dictionary("stroke" -> "black", "fill" -> "none", "id" -> "boundary-circle"))
    var startPoint = s.circle(0.4*displayWidth, 0.4*displayHeight, 16).attr(
      js.Dictionary("fill" -> "red", "id" -> "end-point-1"))
    var endPoint = s.circle(0.6*displayWidth, 0.4*displayHeight, 16).attr(
      js.Dictionary("fill" -> "red", "id" -> "end-point-2"))
    startPoint.drag(
      ((el: js.Dynamic, dX: Int, dY: Int, posX: Int, posY: Int) => 
        {
          val cx = (el.ox.toInt+dX.toInt)
          val cy = (el.oy.toInt+dY.toInt)
          println(el.ox)
          el.attr(js.Dictionary("cx" -> cx, "cy" -> cy))}): js.ThisFunction, 
      ((el: js.Dynamic, x: Int, y: Int) => 
        {
          el.ox = el.attr("cx")
          el.oy = el.attr("cy")}): js.ThisFunction, 
      null)
  }
}
