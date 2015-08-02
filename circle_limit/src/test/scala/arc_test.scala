package circle_limit

import utest._
import breeze.math._

object ArcTestSuite extends TestSuite{
  val tests = TestSuite{
    "test Arc can be initiated"-{
      val arc = Arc(1.0+2.0*i, 3.0+4.0*i, 5.0+6.0*i)
    }
    "values are accesible"-{
      val arc = Arc(1.0+2.0*i, 3.0+4.0*i, 5.0+6.0*i)
      arc.start == 1.0+2.0*i
      arc.finish == 3.0+4.0*i
      arc.centre == 5.0+6.0*i
    }
  }
}
