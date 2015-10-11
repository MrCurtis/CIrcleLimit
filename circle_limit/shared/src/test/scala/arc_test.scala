package circle_limit

import utest._
import breeze.math._


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



