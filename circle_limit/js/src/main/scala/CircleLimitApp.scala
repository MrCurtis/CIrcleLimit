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
import circle_limit.graphics.{Converter, Box, Vector}

@JSExport
object CircleLimitApp {
  @JSExport
  def main(): Unit = {

    val displayWidth = 880
    val displayHeight = 880
    val converter = Converter(
      Box(-1.0, -1.0, 2.0, 2.0),
      Box(0.0, 0.0, displayWidth, displayHeight)
    )
    val transformArc = converter.convertArcToSvg _
    val transformLine = converter.convertLineToSvg _
    val toMathematical = converter.convertFromGraphicalToMathematicalSpace _

    val document = js.Dynamic.global.document

    var s = js.Dynamic.global.Snap(displayWidth, displayHeight)
    var bigCircle = s.circle(displayWidth/2, displayHeight/2, 440).attr(
      js.Dictionary("stroke" -> "black", "fill" -> "none", "id" -> "boundary-circle"))
    var startPoint = s.circle(0.4*displayWidth, 0.4*displayHeight, 16).attr(
      js.Dictionary("fill" -> "red", "id" -> "end-point-1"))
    var endPoint = s.circle(0.6*displayWidth, 0.4*displayHeight, 16).attr(
      js.Dictionary("fill" -> "red", "id" -> "end-point-2"))
    var geodesic = s.path("M 100, 100 A 100, 100, 0, 0, 0, 150, 150").attr(
      js.Dictionary("stroke" -> "black", "fill" -> "none", "id" -> "geodesic"))
    var startPointX = "0.0"
    var startPointY = "0.0"
    var endPointX = "0.0"
    var endPointY = "0.0"
    startPoint.drag(
      ((el: js.Dynamic, dX: Int, dY: Int, posX: Int, posY: Int) => 
        {
          val cx = (startPointX.toInt+dX.toInt)
          val cy = (startPointY.toInt+dY.toInt)
          el.attr(js.Dictionary("cx" -> cx, "cy" -> cy))

          val startPointVector = Vector(startPoint.attr("cx").toString.toDouble, startPoint.attr("cy").toString.toDouble)
          val endPointVector = Vector(endPoint.attr("cx").toString.toDouble, endPoint.attr("cy").toString.toDouble)
          val curve = Geodesic(
            toMathematical(startPointVector),
            toMathematical(endPointVector),
            SpaceType.PoincareDisc
          ).asCurve
          val svg = curve match {
            case arc: Arc => transformArc(arc)
            case line: Line => transformLine(line)
          }
          geodesic.attr(js.Dictionary("d" -> svg))
        }): js.ThisFunction, 
      ((el: js.Dynamic, x: Int, y: Int) => 
        {
          startPointX = el.attr("cx").toString
          startPointY = el.attr("cy").toString
        }): js.ThisFunction, 
      null)
    endPoint.drag(
      ((el: js.Dynamic, dX: Int, dY: Int, posX: Int, posY: Int) => 
        {
          val cx = (endPointX.toInt+dX.toInt)
          val cy = (endPointY.toInt+dY.toInt)
          el.attr(js.Dictionary("cx" -> cx, "cy" -> cy))

          val startPointVector = Vector(startPoint.attr("cx").toString.toDouble, startPoint.attr("cy").toString.toDouble)
          val endPointVector = Vector(endPoint.attr("cx").toString.toDouble, endPoint.attr("cy").toString.toDouble)
          val curve = Geodesic(
            toMathematical(startPointVector),
            toMathematical(endPointVector),
            SpaceType.PoincareDisc
          ).asCurve
          val svg = curve match {
            case arc: Arc => transformArc(arc)
            case line: Line => transformLine(line)
          }
          geodesic.attr(js.Dictionary("d" -> svg))
        }): js.ThisFunction, 
      ((el: js.Dynamic, x: Int, y: Int) => 
        {
          endPointX = el.attr("cx").toString
          endPointY = el.attr("cy").toString
        }): js.ThisFunction, 
      null)
  }
}
