package circle_limit.maths

import utest._
import spire.math.{
  Complex
}
import spire.implicits._
import Imaginary.i

object SpaceTypesTestSuite extends TestSuite {
  val tests = TestSuite {

    "has the right attributes"-{
      List(
        SpaceType.UpperHalfPlane,
        SpaceType.PoincareDisc
      )
      assert (true)
    }

  }
}



