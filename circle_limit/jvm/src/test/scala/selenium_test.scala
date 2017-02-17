package circle_limit

import utest._
import org.openqa.selenium.firefox.FirefoxDriver;

object SeleniumTestSuite extends TestSuite {

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
  }
}
