package circle_limit

import scala.math.{pow, sqrt, round}
import collection.JavaConverters._

import spire.math.Complex
import spire.implicits._
import org.openqa.selenium.{By, WebElement}
import org.openqa.selenium.remote.RemoteWebDriver
import org.openqa.selenium.interactions.Actions

import circle_limit.maths.{Curve, Arc, Line, Geodesic, SpaceType}
import circle_limit.graphics.{Vector, Converter}
import circle_limit.maths.CircleImplicits._

case class CircleAttributes(x: Int, y: Int, r: Double)

case class PageObjectException(err_msg: String) extends Exception(err_msg)

class PageObject(driver: RemoteWebDriver, converter: Converter) {

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
    val handles = driver.findElements(By.className("handle")).asScala.toList
    val handlesAtPointGraphical = handles.filter(isElementAtLocation(pointGraphical)(_))
    assert (
      !handlesAtPointGraphical.isEmpty,
      ("No handle found at mathematical point %s, which corresponds to graphical point %s."
      + " Handles exist with the following attributes: %s").format(
        z,
        pointGraphical,
        handles.map(getCircleAttributes).mkString(", ")))
    this
  }

  def dragHandleFromMathematicalPointToPoint(initial: Complex[Double], destination: Complex[Double]) = {
    val handle = getHandleAtMathematicalPoint(initial)
    dragHandleToMathematicalPoint(handle, destination)
    this
  }

  def assertGeodesicPlottedWithMathematicalEndpoints(z: Complex[Double], w: Complex[Double], exact: Boolean=false) = {
    val expectedCurve = Geodesic(z, w, SpaceType.PoincareDisc).asCurve
    val actualCurves = getGeodesicsAsCurves
    if (exact) {
      assert(
        actualCurves
          .exists( _ == expectedCurve ),
          "Curve %s not plotted exactly. Actual curves plotted: %s".format(
            expectedCurve, actualCurves.mkString(", ")))
    } else {
      assert(
        actualCurves
          .exists(curvesAlmostEqual(_, expectedCurve)),
          "No curve approximating %s plotted. Actual curves plotted: %s".format(
            expectedCurve, actualCurves.mkString(", ")))
    }
    this
  }

  def singleClickAtMathematicalPoint(z: Complex[Double]) = {
    val graphicalPoint = converter.convertFromMathematicalToGraphicalSpace(z)
    val svgElement = driver.findElement(By.tagName("svg"))
    new Actions(driver)
      .moveToElement(svgElement, round(graphicalPoint.x.toFloat), round(graphicalPoint.y.toFloat))
      .click()
      .perform()
    this
  }

  def doubleClickAtMathematicalPoint(z: Complex[Double]) = {
    val graphicalPoint = converter.convertFromMathematicalToGraphicalSpace(z)
    val svgElement = driver.findElement(By.tagName("svg"))
    new Actions(driver)
      .moveToElement(svgElement, round(graphicalPoint.x.toFloat), round(graphicalPoint.y.toFloat))
      .click() // This appears to necessary to emulate a double click properly.
      .doubleClick()
      .perform()
    this
  }

  def tripleClickAtMathematicalPoint(z: Complex[Double]) = {
    // TODO - This method 'works' - at least for current purposes. However I'm pretty certain that
    // it does not trigger the correct number of mouseUp and mouseDown event. Same goes for
    // doubleClickAtMathematicalPoint above.
    val graphicalPoint = converter.convertFromMathematicalToGraphicalSpace(z)
    val svgElement = driver.findElement(By.tagName("svg"))
    val svgElementPosition = svgElement.getLocation()
    val posX = round(graphicalPoint.x.toFloat) + svgElementPosition.x
    val posY = round(graphicalPoint.y.toFloat) + svgElementPosition.y
    val elementToClick = driver.executeScript(
      "return document.elementFromPoint(arguments[0][0], arguments[0][1]);",
      List(posX, posY).asJava)
    val jsTripleClick = """
      arguments[0][0].dispatchEvent(
        new MouseEvent(
          'click',
          {detail: 3, clientX: arguments[0][1], clientY: arguments[0][2]}));"""
    new Actions(driver)
      .moveToElement(svgElement, round(graphicalPoint.x.toFloat), round(graphicalPoint.y.toFloat))
      .click() // This appears to necessary to emulate a double click properly.
      .doubleClick()
      .perform()
    driver.executeScript(
      jsTripleClick,
      List(
        elementToClick,
        round(graphicalPoint.x.toFloat),
        round(graphicalPoint.y.toFloat)).asJava)
    this
  }

  def createGeodesicWithHandlesAtMathematicalPoints(z: Complex[Double], w: Complex[Double]) = {
    doubleClickAtMathematicalPoint(z)
    doubleClickAtMathematicalPoint(w)
    this
  }

  def createMultiGeodesicWithHandlesAtMathematicalPoints(points: List[Complex[Double]]) = {
    def plotRecurse(points: List[Complex[Double]]): Unit = {
      points match {
        case Nil => throw PageObjectException("Cannot create multi-geodesic from less than two points")
        case z :: Nil => doubleClickAtMathematicalPoint(z)
        case z :: zs => {
          singleClickAtMathematicalPoint(z)
          plotRecurse(zs)
        }
      }
    }
    doubleClickAtMathematicalPoint(points.head)
    plotRecurse(points.tail)
    this
  }

  def assertNumberOfGeodesicsPlotted(expectedNumber: Int) = {
    val actualNumber = getGeodesicsAsCurves.length
    assert (
      expectedNumber == actualNumber,
      "Expected number of geodesics plotted: %d. Actual: %d".format(expectedNumber, actualNumber))
    this
  }

  def assertNumberOfHandlesPlotted(expectedNumber: Int) = {
    val actualNumber = driver.findElements(By.className("handle")).asScala.toList.length
    assert (
      expectedNumber == actualNumber,
      "Expected number of handles plotted: %d. Actual: %d".format(expectedNumber, actualNumber))
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
    val graphicalPoint = converter.convertFromMathematicalToGraphicalSpace(point)
    val svgElement = driver.findElement(By.tagName("svg"))
    new Actions(driver)
      .clickAndHold(handle)
      .moveToElement(svgElement, round(graphicalPoint.x.toFloat), round(graphicalPoint.y.toFloat))
      .release()
      .perform()
  }

  private def getHandleAtMathematicalPoint(z: Complex[Double]) = {
    val pointGraphical = converter.convertFromMathematicalToGraphicalSpace(z)
    val handles = driver.findElements(By.className("handle"))
    val handlesAtPointGraphical = handles.asScala.toList.filter(isElementAtLocation(pointGraphical)(_))
    if (handlesAtPointGraphical.isEmpty)
      throw PageObjectException("No handle at graphical point (%f, %f)".format(pointGraphical.x, pointGraphical.y))
    if (handlesAtPointGraphical.length > 1 )
      throw PageObjectException(
        "More than one handle at graphical point (%f, %f)".format(pointGraphical.x, pointGraphical.y))
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
  def apply(driver: RemoteWebDriver, converter: Converter) = new PageObject(driver, converter)
}
