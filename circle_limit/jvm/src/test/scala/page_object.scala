package circle_limit

import scala.math.{pow, sqrt, round}
import collection.JavaConverters._

import spire.math.Complex
import spire.implicits._
import org.openqa.selenium.{By, WebElement, Dimension}
import org.openqa.selenium.remote.RemoteWebDriver
import org.openqa.selenium.interactions.Actions

import circle_limit.maths.{Curve, Arc, Line, Geodesic, SpaceType}
import circle_limit.graphics.{Box, Vector, Converter}
import circle_limit.maths.CircleImplicits._

case class CircleAttributes(x: Double, y: Double, r: Double)

case class PageObjectException(err_msg: String) extends Exception(err_msg)

class PageObject(driver: RemoteWebDriver) {

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
    val pointGraphical = getCurrentConverter().convertFromMathematicalToGraphicalSpace(z)
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

  def assertBoundaryCircleCentredInViewPort() = {
    val svgElement = driver.findElement(By.id("main-display"))
    val viewportCentreHorizontal = driver.executeScript("return window.innerWidth").toString.toDouble/2.0
    val viewportCentreVertical = driver.executeScript("return window.innerHeight").toString.toDouble/2.0
    val boundaryCircleAttributes = getCircleAttributes(driver.findElement(By.id("boundary-circle")))
    val circleCentreHorizontal = svgElement.getLocation().x + boundaryCircleAttributes.x
    val circleCentreVertical = svgElement.getLocation().y + boundaryCircleAttributes.y

    assert(
      (viewportCentreHorizontal - circleCentreHorizontal).abs <= 1,
      "viewport horizontal centre: %s, circle centre in viewport: %s".format(
        viewportCentreHorizontal, circleCentreHorizontal))
    assert(
      (viewportCentreVertical - circleCentreVertical).abs <= 1,
      "viewport vertical centre: %s, circle vertical centre in viewport: %s".format(
        viewportCentreVertical, circleCentreVertical))

    this
  }

  def assertBoundaryCircleToHeightRatioApproximately(expectedRatio: Double) = {
    val boundaryCircleDiameter = getCircleAttributes(driver.findElement(By.id("boundary-circle"))).r * 2
    val viewportHeight = driver.executeScript("return window.innerHeight").toString.toDouble
    val actualRatio = boundaryCircleDiameter/viewportHeight

    assert (
      (actualRatio - expectedRatio).abs < 0.01,
      "viewport height: %s, boundary circle diameter: %s, ratio: %s".format(
        viewportHeight, boundaryCircleDiameter, actualRatio))

    this
  }

  def assertBoundaryCircleToWidthRatioApproximately(expectedRatio: Double) = {
    val boundaryCircleDiameter = getCircleAttributes(driver.findElement(By.id("boundary-circle"))).r * 2
    val viewportWidth = driver.executeScript("return window.innerWidth").toString.toDouble
    val actualRatio = boundaryCircleDiameter/viewportWidth

    assert (
      (actualRatio - expectedRatio).abs < 0.01,
      "viewport width: %s, boundary circle diameter: %s, ratio: %s".format(
        viewportWidth, boundaryCircleDiameter, actualRatio))

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

  def assertHandleAtGraphicalPoint(x: Int, y: Int) = {
    val graphicalPoint = Vector(x,y)
    val handles = driver.findElements(By.className("handle")).asScala.toList
    val handlesAtPointGraphical = handles.filter(isElementAtLocation(graphicalPoint)(_))
    assert (
      !handlesAtPointGraphical.isEmpty,
      ("No handle found at graphical point %s."
      + " Handles exist with the following attributes: %s").format(
        graphicalPoint,
        handles.map(getCircleAttributes).mkString(", ")))
    this

  }

  def assertGeodesicPlottedWithGraphicalEndpoints(x1: Int, y1: Int, x2: Int, y2: Int) = {
    val toMathematical = getCurrentConverter().convertFromGraphicalToMathematicalSpace _
    val pointMathematical1 = toMathematical(Vector(x1.toDouble, y1.toDouble))
    val pointMathematical2 = toMathematical(Vector(x2.toDouble, y2.toDouble))
    assertGeodesicPlottedWithMathematicalEndpoints(pointMathematical1, pointMathematical2)
    this
  }

  /**
   * Resizes the browser window. Note that the browsers height includes thing like the URL bar and 
   * other controls. If you wish to instead specify the dimensions of the viewport - the portion of
   * the browser in which the content is displayed - use resizeViewport instead.
   */
  def resizeWindow(width: Int, height: Int) = {
    driver.manage().window().setSize(new Dimension(width, height))
    this
  }

  /**
   * Resizes the viewport - the portion of the browser in which the content is displayed.
   */
  def resizeViewport(width: Int, height: Int, tries: Int=5): this.type = {
    val currentInnerWidth = driver.executeScript("return window.innerWidth").toString.toInt
    val currentInnerHeight = driver.executeScript("return window.innerHeight").toString.toInt
    if (currentInnerWidth == width && currentInnerHeight == height) {
      this
    } else if (tries == 0) {
      throw PageObjectException("Cannot resize viewport to correct dimensions")
    } else {
      val currentOuterWidth = driver.executeScript("return window.outerWidth").toString.toInt
      val currentOuterHeight = driver.executeScript("return window.outerHeight").toString.toInt
      val widthDifference = currentOuterWidth - currentInnerWidth
      val heightDifference = currentOuterHeight - currentInnerHeight
      resizeWindow(width+widthDifference, height+heightDifference)
      resizeViewport(width, height, tries-1)
    }
  }

  def doubleClickAtGraphicalPoint(x: Int, y: Int) = {
    val svgElement = driver.findElement(By.tagName("svg"))
    new Actions(driver)
      .moveToElement(svgElement, x, y)
      .click() // This appears to necessary to emulate a double click properly.
      .doubleClick()
      .perform()
    this
  }

  def singleClickAtMathematicalPoint(z: Complex[Double]) = {
    val graphicalPoint = getCurrentConverter().convertFromMathematicalToGraphicalSpace(z)
    val svgElement = driver.findElement(By.tagName("svg"))
    new Actions(driver)
      .moveToElement(svgElement, round(graphicalPoint.x.toFloat), round(graphicalPoint.y.toFloat))
      .click()
      .perform()
    this
  }

  def doubleClickAtMathematicalPoint(z: Complex[Double]) = {
    val graphicalPoint = getCurrentConverter().convertFromMathematicalToGraphicalSpace(z)
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
    val graphicalPoint = getCurrentConverter().convertFromMathematicalToGraphicalSpace(z)
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

  //TODO -This is a cut-n-paste-n-modify from CircleLimitApp. In particular if the mathematical box
  //is altered in the app code then it needs to be changed here.
  private def getCurrentConverter() = {
    val svgElement = driver.findElement(By.id("main-display"))
    val svgWidth = svgElement.getSize().width
    val svgHeight = svgElement.getSize().height
    Converter(
      Box(-1.05, -1.05, 2.10, 2.10),
      Box(0.0, 0.0, svgWidth, svgHeight)
    )
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
    val graphicalPoint = getCurrentConverter().convertFromMathematicalToGraphicalSpace(point)
    val svgElement = driver.findElement(By.tagName("svg"))
    new Actions(driver)
      .clickAndHold(handle)
      .moveToElement(svgElement, round(graphicalPoint.x.toFloat), round(graphicalPoint.y.toFloat))
      .release()
      .perform()
  }

  private def getHandleAtMathematicalPoint(z: Complex[Double]) = {
    val pointGraphical = getCurrentConverter().convertFromMathematicalToGraphicalSpace(z)
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
      circle.getAttribute("cx").toDouble,
      circle.getAttribute("cy").toDouble,
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
      .map(getCurrentConverter().convertSvgToCurve)
  }
}

object PageObject {
  def apply(driver: RemoteWebDriver) = new PageObject(driver)
}
