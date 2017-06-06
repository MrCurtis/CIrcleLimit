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
  val allHandleLists = ListBuffer[ListBuffer[js.Dynamic]]()
  var currentHandles = ListBuffer[js.Dynamic]()
  var currentlyDrawing = false

  @JSExport
  def main(): Unit = {

    var svgElement = js.Dynamic.global.Snap(displayWidth, displayHeight)
    createBoundaryCircle(svgElement)
    setUpCreateHandleHandler(svgElement)
  }

  private def createBoundaryCircle(svgElement: js.Dynamic) = {
    svgElement.circle(displayWidth/2, displayHeight/2, 440).attr(
          js.Dictionary("stroke" -> "black", "fill" -> "none", "id" -> "boundary-circle"))
  }

  private def setUpCreateHandleHandler(svgElement: js.Dynamic) = {
    svgElement.click(
      (event: js.Dynamic) => {
        if (!currentlyDrawing && event.detail == 2) {
          val handle = svgElement.circle(event.clientX, event.clientY, 4).attr(
            js.Dictionary("fill" -> "red", "class" -> "handle"))
          currentHandles = ListBuffer[js.Dynamic]()
          allHandleLists += currentHandles
          currentHandles.append(handle)
          refreshHandleHandlers(svgElement)
          refreshGeodesics(svgElement)
          currentlyDrawing = true
        } else if (currentlyDrawing && event.detail == 1){
          val handle = svgElement.circle(event.clientX, event.clientY, 4).attr(
            js.Dictionary("fill" -> "red", "class" -> "handle"))
          currentHandles.append(handle)
          refreshHandleHandlers(svgElement)
          refreshGeodesics(svgElement)
        } else if (currentlyDrawing && event.detail == 2) {
          currentlyDrawing = false
        }
      }
    )
  }

  private def refreshHandleHandlers(svgElement: js.Dynamic) = {
    var tempStoreX = "0.0"
    var tempStoreY = "0.0"
    svgElement.selectAll("[class=handle]").forEach(
      (el: js.Dynamic) => {
        el.undrag()
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
        el.unclick()
        el.click(
          (event: js.Dynamic) => {
            if (!currentlyDrawing) {
              event.stopPropagation()
            }
            if (event.detail == 3){
              el.remove()
              val handleList = getListContainingHandle(el)
              handleList -= el
              if (handleList.length < 2) {
                handleList.foreach( _.remove() )
                allHandleLists -= handleList
              }
            }
            refreshGeodesics(svgElement)
          }
        )
      }
    )
  }

  private def getListContainingHandle(handle: js.Dynamic) = {
    allHandleLists.find(_.contains(handle)).get
  }

  private def refreshGeodesics(svgElement: js.Dynamic) = {
    var boundaryCircle = svgElement.select("[id=boundary-circle]")
    svgElement.selectAll("[class=geodesic]").forEach(
      (el: js.Dynamic) => {
        el.remove()
      }
    )
    allHandleLists.foreach(
      (handles: ListBuffer[js.Dynamic]) => {
        handles.tail zip handles foreach {
          (pair: (js.Dynamic, js.Dynamic)) => {
            val vector1 = Vector(
              pair._1.attr("cx").toString.toDouble,
              pair._1.attr("cy").toString.toDouble)
            val vector2 = Vector(
              pair._2.attr("cx").toString.toDouble,
              pair._2.attr("cy").toString.toDouble)
            val curve = Geodesic(
              toMathematical(vector1),
              toMathematical(vector2),
              SpaceType.PoincareDisc
            ).asCurve
            val geodesic = svgElement.path(convertCurveToSvg(curve)).attr(
              js.Dictionary("stroke" -> "black", "fill" -> "none",  "class" -> "geodesic"))
            // We insert after the boundary circle element in the markup to ensure that handles are
            // rendered 'above' the geodesics.
            geodesic.insertAfter(boundaryCircle)
          }
        }
      }
    )
  }
}
