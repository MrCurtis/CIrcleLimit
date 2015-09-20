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

    "has coherent equality:"-{
      val p1 = new ProjectiveComplex(1.0+2.0*i, 3.0+4.0*i)
      val p2 = new ProjectiveComplex(2.0+4.0*i, 6.0+8.0*i)
      val p3 = new ProjectiveComplex(-4.0+2.0*i, -8.0+6.0*i)
      val p4 = new ProjectiveComplex(1.0+2.1*i, 3.0+4.0*i)

      "== is defined up to ratio of z and w"-{
        assert (p1 == p2)
        assert (p2 == p1)
        assert (p1 == p3)
        assert (p3 == p1)
        assert (p1 != p4)
        assert (p4 != p1)
      }

      "equality implies hashCode equality"-{
        val points = List(p1, p2, p3, p4)
        for (p <- points; q <- points) 
          assert ( (p != q) || (p.hashCode == q.hashCode) )
      }
      
    }

    "returns ExtendedComplex objects of the right type when asExtendedComplex called"-{
      val infinite = new ProjectiveComplex((130.8 + 24.6*i, 0.0 + 0.0*i))
      assert (infinite.toExtendedComplex == Left(Infinity))
      val complex = new ProjectiveComplex((130.8 + 24.6*i, 2.0 + 0.0*i))
      assert (complex.toExtendedComplex == Right(65.4 + 12.3*i))
    }

  }
}

