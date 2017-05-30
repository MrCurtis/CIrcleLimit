package circle_limit

import scala.collection.mutable.ListBuffer
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

  val displayWidth = 880
  val displayHeight = 880
  val converter = Converter(
    Box(-1.0, -1.0, 2.0, 2.0),
    Box(0.0, 0.0, displayWidth, displayHeight)
  )
  val convertCurveToSvg = converter.convertCurveToSvg _
  val toMathematical = converter.convertFromGraphicalToMathematicalSpace _
  val allHandlePairs = ListBuffer[js.Array[js.Dynamic]]()
  var currentHandles = new js.Array[js.Dynamic]

  @JSExport
  def main(): Unit = {

    val document = js.Dynamic.global.document
    var svgElement = js.Dynamic.global.Snap(displayWidth, displayHeight)
    var boundaryCircle = svgElement.circle(displayWidth/2, displayHeight/2, 440).attr(
      js.Dictionary("stroke" -> "black", "fill" -> "none", "id" -> "boundary-circle"))
    setUpCreateHandleHandler(svgElement)
  }

  private def setUpCreateHandleHandler(svgElement: js.Dynamic) = {
    svgElement.dblclick(
      (event: js.Dynamic) => {
        val handle = svgElement.circle(event.clientX, event.clientY, 4).attr(
          js.Dictionary("fill" -> "red", "class" -> "handle"))
        currentHandles.push(handle)
        if (currentHandles.length == 2){
          allHandlePairs += currentHandles
          currentHandles = new js.Array[js.Dynamic]
          refreshHandleHandlers(svgElement)
          refreshGeodesics(svgElement)
        }
      }
    )
  }

  private def refreshHandleHandlers(svgElement: js.Dynamic) = {
    var tempStoreX = "0.0"
    var tempStoreY = "0.0"
    svgElement.selectAll("[class=handle]").forEach(
      (el: js.Dynamic) => {
        el.drag(
          ((event: js.Dynamic, dX: Int, dY: Int, posX: Int, posY: Int) =>
            {
              val cx = (tempStoreX.toInt+dX.toInt)
              val cy = (tempStoreY.toInt+dY.toInt)
              el.attr(js.Dictionary("cx" -> cx, "cy" -> cy))
              refreshGeodesics(svgElement)
            }): js.ThisFunction,
          ((event: js.Dynamic, x: Int, y: Int) =>
            {
              tempStoreX = el.attr("cx").toString
              tempStoreY = el.attr("cy").toString
            }): js.ThisFunction,
          ((event: js.Dynamic) =>
            {
            }): js.ThisFunction
        )
      }
    )
  }

  private def refreshGeodesics(svgElement: js.Dynamic) = {
    svgElement.selectAll("[class=geodesic]").forEach(
      (el: js.Dynamic) => {
        el.remove()
      }
    )
    allHandlePairs.foreach(
      (pair: js.Array[js.Dynamic]) => {
        val vector1 = Vector(
          pair(0).attr("cx").toString.toDouble,
          pair(0).attr("cy").toString.toDouble)
        val vector2 = Vector(
          pair(1).attr("cx").toString.toDouble,
          pair(1).attr("cy").toString.toDouble)
        val curve = Geodesic(
          toMathematical(vector1),
          toMathematical(vector2),
          SpaceType.PoincareDisc
        ).asCurve
        svgElement.path(convertCurveToSvg(curve)).attr(
          js.Dictionary("stroke" -> "black", "fill" -> "none",  "class" -> "geodesic"))
      }
    )
  }
}
