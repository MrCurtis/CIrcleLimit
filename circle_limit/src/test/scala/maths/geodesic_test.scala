package circle_limit

import utest._
import breeze.math.i


object GeodesicTestSuite extends TestSuite{
  val tests = TestSuite{
    import CircleImplicits._

    "can be instantiated"-{
      val geodesic = Geodesic(1.0 + 2.0*i, 3.0 + 4.0*i, SpaceType.UpperHalfPlane)
    }

    "returns an Arc for z1=0+5i, z2=3+4i, in upper half plane model"-{
      val geodesic = Geodesic(0.0+5.0*i, 3.0+4.0*i, SpaceType.UpperHalfPlane)
      val arc = Arc(3.0+4.0*i, 0.0+5.0*i, 0.0+0.0*i)
      assert (geodesic.asCurve == arc)
    }

    "returns a Line for z1=1+5i, z2=1+7i, in upper half plane model"-{
      val geodesic = Geodesic(1.0+5.0*i, 1.0+7.0*i, SpaceType.UpperHalfPlane)
      val line = Line(1.0+5.0*i, 1.0+7.0*i)
      assert (geodesic.asCurve == line)
    }

  }
}
