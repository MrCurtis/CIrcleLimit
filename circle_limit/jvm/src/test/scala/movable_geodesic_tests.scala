package circle_limit

import scala.math.{pow, sqrt, round}

import spire.math.Complex
import spire.implicits._
import utest._
import org.openqa.selenium.{By, WebElement}
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.interactions.Actions

import circle_limit.maths.Arc
import circle_limit.graphics.{Converter, Box, Vector}

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
      "be centred at (110, 110)" - {
        assert (round(boundaryCircleAttributes.x) == 110)
        assert (round(boundaryCircleAttributes.y) == 110)
      }
      "have a radius of 100" - {
        assert (boundaryCircleAttributes.r == 100)
      }
    }
    "geodesic with both endpoint in top left quadrant should plot a circular arc"-{
      def dragCentreOfCircleToMathematicalPoint(
          driver: FirefoxDriver,
          id: String,
          point: Complex[Double],
          converter: Converter) = {
        val bodyElement = driver.findElement(By.tagName("svg"))
        val bodyPosition = bodyElement.getLocation
        println("The location of the body element: %s".format(bodyPosition))
        val circle = driver.findElement(By.id(id))
        val circleRadius = circle.getAttribute("r")
        val position = circle.getLocation
        println("The location before dragging: %s".format(position))
        val relativePosition = Vector(position.x-bodyPosition.x, position.y-bodyPosition.y)
        val relativeDestination = converter.convertFromMathematicalToGraphicalSpace(point)
        val distanceToMoveX = relativeDestination.x - relativePosition.x
        val distanceToMoveY = relativeDestination.y - relativePosition.y
        var builder = new Actions(driver)
        builder.dragAndDropBy(circle, round(distanceToMoveX.toFloat), round(distanceToMoveY.toFloat)).perform()  
        val circle2 = driver.findElement(By.id(id))
        println("The location after dragging: %s".format(circle2.getLocation))
      }
      val driver = new FirefoxDriver()
      val displayWidth = 220
      val displayHeight = 220
      val converter = Converter(
        Box(-1.0, -1.0, 2.0, 2.0),
        Box(0.0, 0.0, displayWidth, displayHeight))
      val toGraphical = converter.convertFromMathematicalToGraphicalSpace _
      val toArc = converter.convertSvgToArc _
      val startPoint = Complex((sqrt(3)-2.0)/2.0, 1.0/2.0)
      val finishPoint = Complex(-1.0/2.0, (2-sqrt(3))/2.0)
      val centre = Complex(-1.0, 1.0)
      val expectedArc = Arc(startPoint, finishPoint, centre)
      val returnedArc = try {
          var builder = new Actions(driver)
          driver.get(fileUrl)
          val endPoint1 = driver.findElement(By.id("end-point-1"))
          val endPoint2 = driver.findElement(By.id("end-point-2"))
          dragCentreOfCircleToMathematicalPoint(driver, "end-point-1", Complex(1.0, 2.0), converter)
          val endPoint1Destination = toGraphical(startPoint)
          val endPoint2Destination = toGraphical(finishPoint)
          val geodesic = driver.findElement(By.id("geodesic"))
          toArc(geodesic.getAttribute("d"))
        } finally {
          driver.close()
        }
      assert((returnedArc.start-expectedArc.start).abs < 0.0001 )
      assert((returnedArc.finish-expectedArc.finish).abs < 0.0001 )
      assert((returnedArc.centre-expectedArc.centre).abs < 0.0001 )
    } 
    "geodesic with both points on imaginary line should plot a line"-{
      assert(false)
    }
  }

  def getCircleAttributes(circle: WebElement) = {
    CircleAttributes(
      circle.getAttribute("cx").toInt,
      circle.getAttribute("cy").toInt,
      circle.getAttribute("r").toDouble
    )
  }

}
