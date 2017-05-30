package circle_limit

import scala.math.{pow, sqrt, round}

import spire.math.Complex
import spire.implicits._
import utest._
import org.openqa.selenium.{By, WebElement, Dimension}
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.interactions.Actions

import org.openqa.selenium.firefox.FirefoxDriver

import circle_limit.maths.{Arc, Geodesic, SpaceType, Line}
import circle_limit.graphics.{Converter, Box, Vector}
import circle_limit.maths.CircleImplicits._


abstract class AcceptanceTestSuite extends TestSuite {

  val fileUrl = "file:///vagrant/circle_limit/circle_limit.html"

  def loadPage(driver: FirefoxDriver) = {
    val displayWidth = 880
    val displayHeight = 880
    val converter = Converter(
      Box(-1.0, -1.0, 2.0, 2.0),
      Box(0.0, 0.0, displayWidth, displayHeight))
    driver.get(fileUrl)
    driver.manage().window().setSize(new Dimension(1024, 1024))
    PageObject(driver, converter)
  }

}


object InitialPageLayoutTests extends AcceptanceTestSuite {

  val tests = TestSuite {

    "title should be \"Circle Limit\""-{
      val driver = new FirefoxDriver()
      try{
        loadPage(driver)
          .assertTitle("Circle Limit")
      } finally {
        driver.close()
      }
    }
    "border circle should have correct position and radius" - {
      val driver = new FirefoxDriver()
      try{
        loadPage(driver)
          .assertBoundaryCircleCentredAtGraphicalPoint(440, 440)
          .assertBoundaryCircleHasGraphicalRadius(440)
      } finally {
        driver.close()
      }
    }
    "no geodesics should be displayed" - {
      val driver = new FirefoxDriver()
      try{
        loadPage(driver)
          .assertNumberOfGeodesicsPlotted(0)
      } finally {
        driver.close()
      }
    }
  }

}

object MovableGeodisicTests extends AcceptanceTestSuite {

  val tests = TestSuite {

    "double clicking on two points should create a geodesic with handle points" - {
      val point1 = Complex(0.4, -0.1)
      val point2 = Complex(0.2, 0.5)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          .doubleClickAtMathematicalPoint(point1)
          .doubleClickAtMathematicalPoint(point2)
          .assertHandlePointLocatedAtMathematicalPoint(point1)
          .assertHandlePointLocatedAtMathematicalPoint(point2)
          .assertGeodesicPlottedWithMathematicalEndpoints(point1, point2)
      } finally {
        driver.close()
      }
    }
    "should be able to create multiple geodesics by double clicking" - {
      val pointA1 = Complex(0.4, -0.1)
      val pointA2 = Complex(0.2, 0.5)
      val pointB1 = Complex(0.7, 0.1)
      val pointB2 = Complex(0.1, -0.5)
      val pointC1 = Complex(-0.2, -0.5)
      val pointC2 = Complex(0.3, 0.6)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          .doubleClickAtMathematicalPoint(pointA1)
          .doubleClickAtMathematicalPoint(pointA2)
          .assertHandlePointLocatedAtMathematicalPoint(pointA1)
          .assertHandlePointLocatedAtMathematicalPoint(pointA2)
          .assertGeodesicPlottedWithMathematicalEndpoints(pointA1, pointA2)
          .doubleClickAtMathematicalPoint(pointB1)
          .doubleClickAtMathematicalPoint(pointB2)
          .assertHandlePointLocatedAtMathematicalPoint(pointB1)
          .assertHandlePointLocatedAtMathematicalPoint(pointB2)
          .assertGeodesicPlottedWithMathematicalEndpoints(pointB1, pointB2)
          .doubleClickAtMathematicalPoint(pointC1)
          .doubleClickAtMathematicalPoint(pointC2)
          .assertHandlePointLocatedAtMathematicalPoint(pointC1)
          .assertHandlePointLocatedAtMathematicalPoint(pointC2)
          .assertGeodesicPlottedWithMathematicalEndpoints(pointC1, pointC2)
          .assertNumberOfGeodesicsPlotted(3)
      } finally {
        driver.close()
      }
    }
    "geodesic with both endpoint in top left quadrant should plot a circular arc"-{
      val point1 = Complex(0.5, -0.5)
      val point2 = Complex(-0.5, -0.5)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          .createGeodesicWithHandlesAtMathematicalPoints(point1, point2)
          .assertGeodesicPlottedWithMathematicalEndpoints(point1, point2)
        } finally {
          driver.close()
        }
    } 
    "geodesic with both points on line through the origin should plot a line segment"-{
      // NOTE - We have chosen end points so that we don't get any floating point errors. Thus we can use exact 
      // equality here. This is important as small errors would result in an arc rather than a line being plotted.
      val point1 = Complex(-0.6, 0.0)
      val point2 = Complex(0.6, 0.0)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          .createGeodesicWithHandlesAtMathematicalPoints(point1, point2)
          .assertGeodesicPlottedWithMathematicalEndpoints(point1, point2, exact=true)
      } finally {
        driver.close()
      }
    }
    "geodesic should be moveable using handles" - {
      val initialPoint1 = Complex(-0.2, 0.2)
      val destinationPoint1= Complex(0.5, -0.5)
      val initialPoint2 = Complex(0.2, 0.2)
      val destinationPoint2 = Complex(-0.5, -0.5)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          .createGeodesicWithHandlesAtMathematicalPoints(initialPoint1, initialPoint2)
          .dragHandleFromMathematicalPointToPoint(initialPoint1, destinationPoint1)
          .dragHandleFromMathematicalPointToPoint(initialPoint2, destinationPoint2)
          .assertHandlePointLocatedAtMathematicalPoint(destinationPoint1)
          .assertHandlePointLocatedAtMathematicalPoint(destinationPoint2)
          .assertGeodesicPlottedWithMathematicalEndpoints(destinationPoint1, destinationPoint2)
      } finally {
        driver.close()
      }
    
    }

  }

}
