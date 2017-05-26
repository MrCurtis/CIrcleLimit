package circle_limit

import scala.math.{pow, sqrt, round}
import collection.JavaConverters._

import spire.math.Complex
import spire.implicits._
import org.openqa.selenium.{By, WebElement}
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.interactions.Actions

import circle_limit.maths.{Curve, Arc, Line, Geodesic, SpaceType}
import circle_limit.graphics.{Vector, Converter}
import circle_limit.maths.CircleImplicits._

case class PageObjectException(err_msg: String) extends Exception(err_msg)

class PageObject(driver: FirefoxDriver, converter: Converter) {

  def assertTitle(expectedTitle: String) = {
    val actualTitle = driver.getTitle
    assert (
      expectedTitle == actualTitle,
      "Expected title: %s. Actual title: %s.".format(expectedTitle, actualTitle))
    this
  }

  def assertBoundaryCircleCentredAtGraphicalPoint(x: Int, y: Int) = {
    val boundaryCircleAttributes = getCircleAttributes(driver.findElement(By.id("boundary-circle")))
    assert (round(boundaryCircleAttributes.x) == x)
    assert (round(boundaryCircleAttributes.y) == y)
    this
  }

  def assertBoundaryCircleHasGraphicalRadius(r: Int) = {
    val boundaryCircleAttributes = getCircleAttributes(driver.findElement(By.id("boundary-circle")))
    assert (boundaryCircleAttributes.r == r)
    this
  }

  def assertHandlePointLocatedAtMathematicalPoint(z: Complex[Double]) = {
    val pointGraphical = converter.convertFromMathematicalToGraphicalSpace(z)
    val handles = driver.findElements(By.className("handle"))
    val handlesAtPointGraphical = handles.asScala.filter(isElementAtLocation(pointGraphical)(_))
    assert (!handlesAtPointGraphical.isEmpty)
    this
  }

  def dragHandleFromMathematicalPointToPoint(initial: Complex[Double], destination: Complex[Double]) = {
    val handle = getHandleAtMathematicalPoint(initial)
    dragHandleToMathematicalPoint(handle, destination)
    this
  }

  def assertGeodesicPlottedWithMathematicalEndpoints(z: Complex[Double], w: Complex[Double], exact: Boolean=false) = {
    val geodesicAsCurve = Geodesic(z, w, SpaceType.PoincareDisc).asCurve
    assert(
      if (exact) {
        getGeodesicsAsCurves
          .exists( _ == geodesicAsCurve )
      } else {
        getGeodesicsAsCurves
          .exists(curvesAlmostEqual(_, geodesicAsCurve))
      }
    )
    this
  }

  def doubleClickAtMathematicalPoint(z: Complex[Double]) = {
    val graphicalPoint = converter.convertFromMathematicalToGraphicalSpace(z)
    val svgElement = driver.findElement(By.tagName("svg"))
    new Actions(driver)
      .moveToElement(svgElement, round(graphicalPoint.x.toFloat), round(graphicalPoint.y.toFloat))
      .doubleClick()
      .pause(1)
      .build()
      .perform()
    this
  }

  //TODO - This should be moved to the test helper object, and tests added.
  private def curvesAlmostEqual(curve1: Curve, curve2: Curve): Boolean = {
    (curve1, curve2) match {
      case (arc1: Arc, arc2: Arc) => arcsAlmostEqual(arc1, arc2)
      case (line1: Line, line2: Line) => linesAlmostEqual(line1, line2)
      case (arc1: Arc, line2: Line) => arcAndLineAlmostEqual(arc1, line2)
      case (line1: Line, arc2: Arc) => arcAndLineAlmostEqual(arc2, line1)
    }  
  }

  private val errorDelta = 0.005

  private def arcsAlmostEqual(arc1: Arc, arc2: Arc) = {
    (
      (((arc1.start - arc2.start).abs < errorDelta && (arc1.finish - arc2.finish).abs < errorDelta)
        || ((arc1.start - arc2.finish).abs < errorDelta && (arc1.finish - arc2.start).abs < errorDelta)) 
          && (arc1.centre - arc2.centre).abs < errorDelta
    )
  }

  private def linesAlmostEqual(line1: Line, line2: Line) = {
    (
      ((line1.start - line2.start).abs < errorDelta && (line1.finish - line2.finish).abs < errorDelta)
        || ((line1.start - line2.finish).abs < errorDelta && (line1.finish - line2.start).abs < errorDelta)
    )
  }

  private def arcAndLineAlmostEqual(arc: Arc, line: Line) = {
    (
      (((arc.start - line.start).abs < errorDelta && (arc.finish - line.finish).abs < errorDelta)
        || ((arc.start - line.finish).abs < errorDelta && (arc.finish - line.start).abs < errorDelta))
          && (arc.centre).abs > 1.0/errorDelta
    )
  
  }

  private def dragHandleToMathematicalPoint(handle: WebElement, point: Complex[Double]) = {
    val handleRadius = handle.getAttribute("r")
    val position = handle.getLocation
    val relativePosition = Vector(position.x, position.y)
    val relativeDestination = Vector(
      converter.convertFromMathematicalToGraphicalSpace(point).x-16,
      converter.convertFromMathematicalToGraphicalSpace(point).y-16) //magic number
    val distanceToMoveX = relativeDestination.x - relativePosition.x
    val distanceToMoveY = relativeDestination.y - relativePosition.y
    var builder = new Actions(driver)
    builder.dragAndDropBy(handle, round(distanceToMoveX.toFloat), round(distanceToMoveY.toFloat)).build().perform() 
  }

  private def getHandleAtMathematicalPoint(z: Complex[Double]) = {
    val pointGraphical = converter.convertFromMathematicalToGraphicalSpace(z)
    val handles = driver.findElements(By.className("handle"))
    val handlesAtPointGraphical = handles.asScala.toList.filter(isElementAtLocation(pointGraphical)(_))
    if (handlesAtPointGraphical.isEmpty)
      throw PageObjectException("No handle at graphical point (%d, %d)".format(pointGraphical.x, pointGraphical.y))
    if (handlesAtPointGraphical.length > 1 )
      throw PageObjectException(
        "More than one handle at graphical point (%d, %d)".format(pointGraphical.x, pointGraphical.y))
    handlesAtPointGraphical.head
  }

  private def getCircleAttributes(circle: WebElement) = {
    CircleAttributes(
      circle.getAttribute("cx").toInt,
      circle.getAttribute("cy").toInt,
      circle.getAttribute("r").toDouble
    )
  }

  private def isElementAtLocation(location: Vector)(element: WebElement) = {
    val attribute = getCircleAttributes(element)
    (location.x - attribute.x).abs < 2 && (location.y - attribute.y).abs < 2
  }

  private def getGeodesicisAsWebElements: List[WebElement] = {
    driver.findElements(By.className("geodesic")).asScala.toList
  }

  private def getGeodesicsAsCurves: List[Curve] = {
    getGeodesicisAsWebElements
      .map(_.getAttribute("d"))
      .map(converter.convertSvgToCurve)
  }
}

object PageObject {
  def apply(driver: FirefoxDriver, converter: Converter) = new PageObject(driver, converter)
}
