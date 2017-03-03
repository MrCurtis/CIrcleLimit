package circle_limit

import Math.{pow, sqrt}

import utest._
import org.openqa.selenium.{By, WebElement}
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.interactions.Actions

case class CircleAttributes(x: Int, y: Int, r: Double)

object SnapJsExperimentTestSuite extends TestSuite {

  val tests = TestSuite {

    val fileUrl = "file:///vagrant/circle_limit/snap_js_experiment.html"

    "title should be \"Snap JS Experiment\""-{
      val driver = new FirefoxDriver()
      try{
        driver.get(fileUrl)
        val expectedTitle = "Snap JS Experiment"
        val actualTitle = driver.getTitle
        assert (expectedTitle == actualTitle)
      } finally {
        driver.close()
      }
    }
    "dragging centre dot should:"-{
      val driver = new FirefoxDriver()
      var builder = new Actions(driver)
      driver.get(fileUrl)
      val centreCircle = driver.findElement(By.id("centre-of-circle"))
      val edgeOfCircle = driver.findElement(By.id("edge-of-circle"))
      val bigCircle = driver.findElement(By.id("big-circle"))
      val initialLocationOfCentreCircle = centreCircle.getLocation()
      val initialLocationOfEdgeOfCircle = edgeOfCircle.getLocation()
      val initialLocationOfBigCircle = bigCircle.getLocation()

      var dragCentreCircle = builder.dragAndDropBy(centreCircle, 10, 20).perform()

      "move the centre dot"-{
        val finalLocationOfCentreCircle = centreCircle.getLocation()
        assert(finalLocationOfCentreCircle.x - initialLocationOfCentreCircle.x == 10)
        assert(finalLocationOfCentreCircle.y - initialLocationOfCentreCircle.y == 20)
      }
      "move the perimeter dot"-{
        val finalLocationOfEdgeOfCircle = edgeOfCircle.getLocation()
        assert(finalLocationOfEdgeOfCircle.x - initialLocationOfEdgeOfCircle.x == 10)
        assert(finalLocationOfEdgeOfCircle.y - initialLocationOfEdgeOfCircle.y == 20)
      }
      "move the circle"-{
        val finalLocationOfBigCircle = bigCircle.getLocation()
        assert(finalLocationOfBigCircle.x - initialLocationOfBigCircle.x == 10)
        assert(finalLocationOfBigCircle.y - initialLocationOfBigCircle.y == 20)
      }
    }
    "dragging perimeter dot should:"-{
      val driver = new FirefoxDriver()
      var builder = new Actions(driver)
      driver.get(fileUrl)
      val centreCircle = driver.findElement(By.id("centre-of-circle"))
      val edgeOfCircle = driver.findElement(By.id("edge-of-circle"))
      val bigCircle = driver.findElement(By.id("big-circle"))
      val initialCentreCircleAttributes = getCircleAttributes(centreCircle)
      val initialEdgeOfCircleAttributes = getCircleAttributes(edgeOfCircle)
      val initialBigCircleAttributes = getCircleAttributes(bigCircle)

      var dragCentreCircle = builder.dragAndDropBy(edgeOfCircle, -5, 12).perform()

      "not move the centre dot"-{
        val finalCentreCircleAttributes = getCircleAttributes(centreCircle)
        assert(initialCentreCircleAttributes.x == finalCentreCircleAttributes.x)
        assert(initialCentreCircleAttributes.y == finalCentreCircleAttributes.y)
      }
      "move the perimeter dot"-{
        val finalEdgeOfCircleAttributes = getCircleAttributes(edgeOfCircle)
        assert(finalEdgeOfCircleAttributes.x - initialEdgeOfCircleAttributes.x == -5)
        assert(finalEdgeOfCircleAttributes.y - initialEdgeOfCircleAttributes.y == 12)
        
      }
      "resize the circle"-{
        val finalCentreCircleAttributes = getCircleAttributes(centreCircle)
        val finalEdgeOfCircleAttributes = getCircleAttributes(edgeOfCircle)
        val finalBigCircleAttributes = getCircleAttributes(bigCircle)
        val expectedRadius = sqrt(pow(finalEdgeOfCircleAttributes.x - finalCentreCircleAttributes.x, 2) + pow(finalEdgeOfCircleAttributes.y - finalCentreCircleAttributes.y, 2))
        val actualRadius = finalBigCircleAttributes.r
        assert (actualRadius < expectedRadius+0.0001)
        assert (actualRadius > expectedRadius-0.0001)
      }
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
