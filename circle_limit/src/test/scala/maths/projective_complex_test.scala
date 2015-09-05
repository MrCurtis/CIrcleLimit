package circle_limit

import utest._
import breeze.math._

object ProjectiveComplexTestSuite extends TestSuite {
  val tests = TestSuite {

    "can be instatiated with a complex number (using implicits)"-{
      import CircleImplicits._
      val z = new ProjectiveComplex(1.0 + 2.0*i)
    }

    "can be instatiated with infinity (using implicits)"-{
      import CircleImplicits._
      val z = new ProjectiveComplex(Infinity)
    }

    "has equality defined by the correct equivalence"-{
      val z = new ProjectiveComplex((1.0 + 2.0*i), (3.0 + 4.0*i))
      val w = new ProjectiveComplex((2.0 + 4.0*i), (6.0 + 8.0*i))
      assert (z == w)
      assert (w == z)
    }

    "returns ExtendedComplex objects of the right type when asExtendedComplex called"-{
      val infinite = new ProjectiveComplex((130.8 + 24.6*i, 0.0 + 0.0*i))
      assert (infinite.toExtendedComplex == Left(Infinity))
      val complex = new ProjectiveComplex((130.8 + 24.6*i, 2.0 + 0.0*i))
      assert (complex.toExtendedComplex == Right(65.4 + 12.3*i))
    }

  }
}

