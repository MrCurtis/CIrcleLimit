package circle_limit
import utest._

import org.openqa.selenium.WebDriver
import org.openqa.selenium.firefox.FirefoxDriver;

object SeleniumTestSuite extends TestSuite {
  val tests = TestSuite {

    "first attempt"-{
      val driver = new FirefoxDriver()
      assert (true)
    }

  }
}
