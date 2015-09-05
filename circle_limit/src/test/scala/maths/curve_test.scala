package circle_limit

import utest._
import breeze.math._

object LineTestSuite extends TestSuite {
  val tests = TestSuite{

    "test Line can be intantiated"-{
      val line = Line(1.0+2.0*i, 3.0+4.0*i)
    }

    "values are accesible"-{
      val line = Line(1.0+2.0*i, 3.0+4.0*i)
      assert (line.start == 1.0+2.0*i)
      assert (line.finish == 3.0+4.0*i)
    }

  }
}

object ArcTestSuite extends TestSuite{
  val tests = TestSuite{

    "test Arc can be intantiated"-{
      val arc = Arc(1.0+2.0*i, 3.0+4.0*i, 5.0+6.0*i)
    }

    "values are accesible"-{
      val arc = Arc(1.0+2.0*i, 3.0+4.0*i, 5.0+6.0*i)
      assert (arc.start == 1.0+2.0*i)
      assert (arc.finish == 3.0+4.0*i)
      assert (arc.centre == 5.0+6.0*i)
    }

  }
}
