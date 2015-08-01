package circle_limit

import utest._
import spire.implicits._
import spire.math.Complex

object ArcTestSuite extends TestSuite{
  val tests = TestSuite{
    "test Arc can be initiated"-{
      val arc = Arc(Complex(1.0,2.0), Complex(3.0,4.0), Complex(5.0,6.0))
    }
  }
}
