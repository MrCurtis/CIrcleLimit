package circle_limit

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.DynamicImplicits.number2dynamic
import org.scalajs.dom.window
import org.scalajs.dom.document
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

class IdCounter {
  var count = 0
  def get() = {
    count = count + 1
    count.toString
  }
}

case class HandleRecord(var mathematicalPosition: Complex[Double], id: String)

@JSExport
object CircleLimitApp {

  val nextId = new IdCounter()
  val svgElementId = "main-display"
  val displayWidth = 880
  val displayHeight = 880
  val converter = Converter(
    Box(-1.0, -1.0, 2.0, 2.0),
    Box(0.0, 0.0, displayWidth, displayHeight)
  )
  val allHandleLists = ListBuffer[ListBuffer[js.Dynamic]]()
  var currentHandles = ListBuffer[js.Dynamic]()
  val handleRecords = Set[HandleRecord]()
  var currentlyDrawing = false

  @JSExport
  def main(): Unit = {

    var svgElement = js.Dynamic.global.Snap().attr(js.Dictionary("id" -> svgElementId))
    setUpResizeHandler(svgElement)
    createBoundaryCircle(svgElement)
    createIterationButton(svgElement)
    setUpCreateHandleHandler(svgElement)
  }

  private def getCurrentConverter() = {
    val svgElement = document.getElementById(svgElementId)
    val svgWidth = svgElement.getBoundingClientRect().width.toDouble
    val svgHeight = svgElement.getBoundingClientRect().height.toDouble
    Converter(
      Box(-1.05, -1.05, 2.10, 2.10),
      Box(0.0, 0.0, svgWidth, svgHeight)
    )
  }

  private def createIterationButton(svgElement: js.Dynamic) = {
    val converter = getCurrentConverter()
    val centre = converter.convertFromMathematicalToGraphicalSpace(Complex(0.95, 0.95))
    val radius = converter.scaleFromMathematicalToGraphicalSpace(0.04)
    val el = svgElement.circle(centre.x, centre.y, radius).attr(
          js.Dictionary("stroke" -> "black", "fill" -> "black", "id" -> "iteration-button"))
    el.click(
      (event: js.Dynamic) => {
        iterateGeodesics(svgElement)
      }
    )
  }

  private def resizeIterationButton(svgElement: js.Dynamic) = {
    val converter = getCurrentConverter()
    val centre = converter.convertFromMathematicalToGraphicalSpace(Complex(0.95, 0.95))
    val radius = converter.scaleFromMathematicalToGraphicalSpace(0.04)
    svgElement.select("[id=iteration-button]").attr(js.Dictionary("cx" -> centre.x, "cy" -> centre.y, "r" -> radius))
  }

  private def createBoundaryCircle(svgElement: js.Dynamic) = {
    val converter = getCurrentConverter()
    val centre = converter.convertFromMathematicalToGraphicalSpace(Complex(0.0, 0.0))
    val radius = converter.scaleFromMathematicalToGraphicalSpace(1.0)
    svgElement.circle(centre.x, centre.y, radius).attr(
          js.Dictionary("stroke" -> "black", "fill" -> "none", "id" -> "boundary-circle"))
  }

  private def resizeBoundaryCircle(svgElement: js.Dynamic) = {
    val converter = getCurrentConverter()
    val centre = converter.convertFromMathematicalToGraphicalSpace(Complex(0.0, 0.0))
    val radius = converter.scaleFromMathematicalToGraphicalSpace(1.0)
    svgElement.select("[id=boundary-circle]").attr(js.Dictionary("cx" -> centre.x, "cy" -> centre.y, "r" -> radius))
  }

  private def setUpCreateHandleHandler(svgElement: js.Dynamic) = {
    svgElement.click(
      (event: js.Dynamic) => {
        if (!currentlyDrawing && event.detail == 2) {
          val handleId = nextId.get()
          val handle = svgElement.circle(event.clientX, event.clientY, 4).attr(
            js.Dictionary("fill" -> "red", "class" -> "handle", "id" -> handleId))
          val posMathematical = getCurrentConverter().convertFromGraphicalToMathematicalSpace(
            Vector(event.clientX.toString.toDouble, event.clientY.toString.toDouble))
          handleRecords += HandleRecord(posMathematical, handleId)
          currentHandles = ListBuffer[js.Dynamic]()
          allHandleLists += currentHandles
          currentHandles.append(handle)
          refreshHandleHandlers(svgElement)
          refreshGeodesics(svgElement)
          currentlyDrawing = true
        } else if (currentlyDrawing && event.detail == 1){
          val handleId = nextId.get()
          val handle = svgElement.circle(event.clientX, event.clientY, 4).attr(
            js.Dictionary("fill" -> "red", "class" -> "handle", "id" -> handleId))
          val posMathematical = getCurrentConverter().convertFromGraphicalToMathematicalSpace(
            Vector(event.clientX.toString.toDouble, event.clientY.toString.toDouble))
          handleRecords += HandleRecord(posMathematical, handleId)
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
              val elementId = el.attr("id").toString
              val posMathematical = getCurrentConverter().convertFromGraphicalToMathematicalSpace(
                Vector(cx.toDouble,cy.toDouble))
              val handleRecord = handleRecords.filter(
                hr => hr match {
                  case HandleRecord(_, `elementId`) => true
                  case _ => false
                }
              ).head.mathematicalPosition = posMathematical
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
              val elementId = el.attr("id").toString
              handleRecords --= handleRecords.filter(
                hr => hr match {
                  case HandleRecord(_, `elementId`) => true
                  case _ => false
                }
              )
              el.remove()
              val handleList = getListContainingHandle(el)
              handleList -= el
              if (handleList.length < 2) {
                handleList.foreach(
                  el => {
                    val elementId = el.attr("id").toString
                    handleRecords --= handleRecords.filter(
                      hr => hr match {
                        case HandleRecord(_, `elementId`) => true
                        case _ => false
                      }
                    )
                    el.remove()
                  }
                )
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

  private def refreshHandles(svgElement: js.Dynamic) = {
    allHandleLists.foreach(
      _.foreach(
        el => {
          val elementId = el.attr("id").toString
          val handleRecord = handleRecords.filter(_.id == elementId).head
          val positionGraphical = getCurrentConverter().convertFromMathematicalToGraphicalSpace(
            handleRecord.mathematicalPosition
          )
          el.attr(js.Dictionary("cx" -> positionGraphical.x.toInt, "cy" -> positionGraphical.y.toInt))
        }
      )
    )
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
              getCurrentConverter().convertFromGraphicalToMathematicalSpace(vector1),
              getCurrentConverter().convertFromGraphicalToMathematicalSpace(vector2),
              SpaceType.PoincareDisc
            ).asCurve
            val geodesic = svgElement.path(getCurrentConverter().convertCurveToSvg(curve)).attr(
              js.Dictionary("stroke" -> "black", "fill" -> "none",  "class" -> "geodesic"))
            // We insert after the boundary circle element in the markup to ensure that handles are
            // rendered 'above' the geodesics.
            geodesic.insertAfter(boundaryCircle)
          }
        }
      }
    )
  }

  private def iterateGeodesics(svgElement: js.Dynamic) = {
    println("Calling iterateGeodesic")
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
              getCurrentConverter().convertFromGraphicalToMathematicalSpace(vector1),
              getCurrentConverter().convertFromGraphicalToMathematicalSpace(vector2),
              SpaceType.PoincareDisc
            )
            val group = Group.torsionFreeGroup(3)
            val images = group.getImagesOfGeodesic(curve)
            images foreach {
              (image: Geodesic) => {
                val geodesic = svgElement.path(getCurrentConverter().convertCurveToSvg(image.asCurve)).attr(
                  js.Dictionary("stroke" -> "black", "fill" -> "none",  "class" -> "geodesic"))
                  geodesic.insertAfter(boundaryCircle)
                }
            }
          }
        }
      }
    )
  }

  private def setUpResizeHandler(svgElement: js.Dynamic) = {
    window.addEventListener("resize", (event: js.Dynamic) => {
      resizeBoundaryCircle(svgElement)
      resizeIterationButton(svgElement)
      refreshHandles(svgElement)
      refreshGeodesics(svgElement)
    })
  }
}
