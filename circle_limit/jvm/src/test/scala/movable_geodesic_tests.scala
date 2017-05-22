package circle_limit

import scala.math.{pow, sqrt, round}

import spire.math.Complex
import spire.implicits._
import utest._
import org.openqa.selenium.{By, WebElement}
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.interactions.Actions

import org.openqa.selenium.firefox.FirefoxDriver

import circle_limit.maths.{Arc, Geodesic, SpaceType, Line}
import circle_limit.graphics.{Converter, Box, Vector}
import circle_limit.maths.CircleImplicits._

object MovableGeodisicTests extends TestSuite {

  val tests = TestSuite {

    val fileUrl = "file:///vagrant/circle_limit/circle_limit.html"

    "title should be \"Circle Limit\""-{
      val driver = new FirefoxDriver()
      try{
        driver.get(fileUrl)
        val expectedTitle = "Circle Limit"
        val actualTitle = driver.getTitle
        assert (expectedTitle == actualTitle)
      } finally {
        driver.close()
      }
    }
    "border circle should" - {
      val driver = new FirefoxDriver()
      val boundaryCircleAttributes = try{
          driver.get(fileUrl)
          getCircleAttributes(driver.findElement(By.id("boundary-circle")))
        } finally {
          driver.close()
        }
      "be centred at (440, 440)" - {
        assert (round(boundaryCircleAttributes.x) == 440)
        assert (round(boundaryCircleAttributes.y) == 440)
      }
      "have a radius of 440" - {
        assert (boundaryCircleAttributes.r == 440)
      }
    }
    "dragging should move end point 1" - {
      val driver = new FirefoxDriver()
      val displayWidth = 880
      val displayHeight = 880
      val converter = Converter(
        Box(-1.0, -1.0, 2.0, 2.0),
        Box(0.0, 0.0, displayWidth, displayHeight))
      val circleAttributesAfterDrag = try {
        driver.get(fileUrl)
        dragCentreOfCircleToMathematicalPoint(
          driver,
          "end-point-1",
          Complex(0.5, 0.0),
          converter)
        getCircleAttributes(driver.findElement(By.id("end-point-1")))
      } finally {
        driver.close()
      }
      assert (circleAttributesAfterDrag.x == 660)
      assert (circleAttributesAfterDrag.y == 440)
    }
    "dragging should move end point 2" - {
      val driver = new FirefoxDriver()
      val displayWidth = 880
      val displayHeight = 880
      val converter = Converter(
        Box(-1.0, -1.0, 2.0, 2.0),
        Box(0.0, 0.0, displayWidth, displayHeight))
      val circleAttributesAfterDrag = try {
        driver.get(fileUrl)
        dragCentreOfCircleToMathematicalPoint(
          driver,
          "end-point-2",
          Complex(0.0, 0.5),
          converter)
        getCircleAttributes(driver.findElement(By.id("end-point-2")))
      } finally {
        driver.close()
      }
      assert (circleAttributesAfterDrag.x == 440)
      assert (circleAttributesAfterDrag.y == 220)
    }
    "geodesic with both endpoint in top left quadrant should plot a circular arc"-{
      val driver = new FirefoxDriver()
      val displayWidth = 880
      val displayHeight = 880
      val converter = Converter(
        Box(-1.0, -1.0, 2.0, 2.0),
        Box(0.0, 0.0, displayWidth, displayHeight))
      val toGraphical = converter.convertFromMathematicalToGraphicalSpace _
      val toArc = converter.convertSvgToArc _
      val startPoint = Complex(0.5, -0.5)
      val finishPoint = Complex(-0.5, -0.5)
      val expectedArc = Geodesic(startPoint, finishPoint, SpaceType.PoincareDisc).asCurve.asInstanceOf[Arc]
      val returnedArc = try {
          driver.manage().window().maximize()
          driver.get(fileUrl)
          val circleStart = driver.findElement(By.id("end-point-1"))
          val circleEnd = driver.findElement(By.id("end-point-2"))
          dragCentreOfCircleToMathematicalPoint(
            driver,
            "end-point-1",
            startPoint,
            converter)
          dragCentreOfCircleToMathematicalPoint(
            driver,
            "end-point-2",
            finishPoint,
            converter)
          val geodesic = driver.findElement(By.id("geodesic"))
          toArc(geodesic.getAttribute("d"))
        } finally {
          driver.close()
        }
      assert((returnedArc.start-expectedArc.start).abs < 0.0005 )
      assert((returnedArc.finish-expectedArc.finish).abs < 0.0005 )
      assert((returnedArc.centre-expectedArc.centre).abs < 0.0005 )
    } 
    "geodesic with both points on line through the origin should plot a line segment"-{
      val driver = new FirefoxDriver()
      val displayWidth = 880
      val displayHeight = 880
      val converter = Converter(
        Box(-1.0, -1.0, 2.0, 2.0),
        Box(0.0, 0.0, displayWidth, displayHeight))
      val toGraphical = converter.convertFromMathematicalToGraphicalSpace _
      val toLine = converter.convertSvgToLine _
      val startPoint = Complex(-0.5, 0.0)
      val finishPoint = Complex(0.5, 0.0)
      val expectedLine = Geodesic(startPoint, finishPoint, SpaceType.PoincareDisc).asCurve.asInstanceOf[Line]
      val returnedLine = try {
          driver.manage().window().maximize()
          driver.get(fileUrl)
          val circleStart = driver.findElement(By.id("end-point-1"))
          val circleEnd = driver.findElement(By.id("end-point-2"))
          dragCentreOfCircleToMathematicalPoint(
            driver,
            "end-point-2",
            finishPoint,
            converter)
          dragCentreOfCircleToMathematicalPoint(
            driver,
            "end-point-1",
            startPoint,
            converter)
          val geodesic = driver.findElement(By.id("geodesic"))
          toLine(geodesic.getAttribute("d"))
        } finally {
          driver.close()
        }
      // NOTE - We have chosen end points so that we don't get any floating point errors. Thus we can use exact 
      // equality here. This is important as small errors would result in an arc rather than a line being plotted.
      assert(returnedLine == expectedLine)
    }
  }

  def getCircleAttributes(circle: WebElement) = {
    CircleAttributes(
      circle.getAttribute("cx").toInt,
      circle.getAttribute("cy").toInt,
      circle.getAttribute("r").toDouble
    )
  }

  def dragCentreOfCircleToMathematicalPoint(
      driver: FirefoxDriver,
      id: String,
      point: Complex[Double],
      converter: Converter) = {
    val circle = driver.findElement(By.id(id))
    val circleRadius = circle.getAttribute("r")
    val position = circle.getLocation
    val relativePosition = Vector(position.x, position.y)
    val relativeDestination = Vector(
      converter.convertFromMathematicalToGraphicalSpace(point).x-16,
      converter.convertFromMathematicalToGraphicalSpace(point).y-16) //magic number
    val distanceToMoveX = relativeDestination.x - relativePosition.x
    val distanceToMoveY = relativeDestination.y - relativePosition.y
    var builder = new Actions(driver)
    builder.dragAndDropBy(circle, round(distanceToMoveX.toFloat), round(distanceToMoveY.toFloat)).build().perform() 
    val circleAfter = driver.findElement(By.id(id))
  }

}
