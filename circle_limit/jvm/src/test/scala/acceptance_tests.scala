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
    "assert that handle point is located in initial position" - {
      val initialHandlePointLocationMathematical = Complex(0.2, 0.2)
      val driver = new FirefoxDriver()
      try{
        loadPage(driver)
          .assertHandlePointLocatedAtMathematicalPoint(initialHandlePointLocationMathematical)
      } finally {
        driver.close()
      }
    }
  }

}

object MovableGeodisicTests extends AcceptanceTestSuite {

  val tests = TestSuite {

    "dragging should move handle point 1" - {
      val initialHandlePointMathematical = Complex(-0.2, 0.2)
      val destinationHandlePointMathematical = Complex(0.5, 0.0)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          .dragHandleFromMathematicalPointToPoint(initialHandlePointMathematical, destinationHandlePointMathematical)
          .assertHandlePointLocatedAtMathematicalPoint(destinationHandlePointMathematical)
      } finally {
        driver.close()
      }
    }
    "dragging should move end point 2" - {
      val initialHandlePointMathematical = Complex(0.2, 0.2)
      val destinationHandlePointMathematical = Complex(0.0, 0.5)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          .dragHandleFromMathematicalPointToPoint(initialHandlePointMathematical, destinationHandlePointMathematical)
          .assertHandlePointLocatedAtMathematicalPoint(destinationHandlePointMathematical)
      } finally {
        driver.close()
      }
    }
    "geodesic with both endpoint in top left quadrant should plot a circular arc"-{
      val initialHandlePointMathematical1 = Complex(-0.2, 0.2)
      val destinationHandlePointMathematical1 = Complex(0.5, -0.5)
      val initialHandlePointMathematical2 = Complex(0.2, 0.2)
      val destinationHandlePointMathematical2 = Complex(-0.5, -0.5)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          .dragHandleFromMathematicalPointToPoint(initialHandlePointMathematical1, destinationHandlePointMathematical1)
          .dragHandleFromMathematicalPointToPoint(initialHandlePointMathematical2, destinationHandlePointMathematical2)
          .assertGeodesicPlottedWithMathematicalEndpoints(
            destinationHandlePointMathematical1,
            destinationHandlePointMathematical2)
        } finally {
          driver.close()
        }
    } 
    "geodesic with both points on line through the origin should plot a line segment"-{
      // NOTE - We have chosen end points so that we don't get any floating point errors. Thus we can use exact 
      // equality here. This is important as small errors would result in an arc rather than a line being plotted.
      val initialHandlePointMathematical1 = Complex(-0.2, 0.2)
      val destinationHandlePointMathematical1 = Complex(-0.5, 0.0)
      val initialHandlePointMathematical2 = Complex(0.2, 0.2)
      val destinationHandlePointMathematical2 = Complex(0.5, 0.0)
      val driver = new FirefoxDriver()
      try {
        loadPage(driver)
          // TODO - Reversing the order of the next two method calls causes the test to fail - the handle at
          // initialHandlePointMathematical2 is not moved. Need to investigate this further.
          .dragHandleFromMathematicalPointToPoint(initialHandlePointMathematical2, destinationHandlePointMathematical2)
          .dragHandleFromMathematicalPointToPoint(initialHandlePointMathematical1, destinationHandlePointMathematical1)
          .assertGeodesicPlottedWithMathematicalEndpoints(
            destinationHandlePointMathematical1,
            destinationHandlePointMathematical2,
            exact=true)
      } finally {
        driver.close()
      }
    }
    "double clicking on two points creates a geodesic with handle points" - {
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

  }

}