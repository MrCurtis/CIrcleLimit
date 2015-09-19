package circle_limit

import utest._
import breeze.math.i


object GeodesicTestSuite extends TestSuite{
  val tests = TestSuite{
    import CircleImplicits._

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

    "returns an Arc for z1=1, z2=i, in Poinare disc model"-{
      val geodesic  = Geodesic(1.0+0.0*i, 0.0+1.0*i, SpaceType.PoincareDisc)
      val arc = Arc(0.0+1.0*i, 1.0+0.0*i, 1.0+1.0*i)
      assert (geodesic.asCurve == arc)
    }

    "returns an Arc for z1=i, z2=1, in Poinare disc model"-{
      val geodesic  = Geodesic(0.0+1.0*i, 1.0+0.0*i, SpaceType.PoincareDisc)
      val arc = Arc(0.0+1.0*i, 1.0+0.0*i, 1.0+1.0*i)
      assert (geodesic.asCurve == arc)
    }

    "returns an Arc for z1=i, z2=-1, in Poinare disc model"-{
      val geodesic  = Geodesic(0.0+1.0*i, -1.0+0.0*i, SpaceType.PoincareDisc)
      val arc = Arc(-1.0+0.0*i, 0.0+1.0*i, -1.0+1.0*i)
      assert (geodesic.asCurve == arc)
    }

    "returns an Arc for z1=-i, z2=-1, in Poinare disc model"-{
      val geodesic  = Geodesic(0.0-1.0*i, -1.0+0.0*i, SpaceType.PoincareDisc)
      val arc = Arc(0.0-1.0*i, -1.0+0.0*i, -1.0-1.0*i)
      assert (geodesic.asCurve == arc)
    }

    "returns a Line for z1=0+0.2i, z2=0+0.8i, in Poincare disc model"-{
      val geodesic = Geodesic(0.0+0.2*i, 0.0+0.8*i, SpaceType.PoincareDisc)
      val line = Line(0.0+0.2*i, 0.0+0.8*i)
      assert (geodesic.asCurve == line)
    }

    "returns a Line for z1=0+0.0i, z2=0.5+0.5i, in Poincare disc model"-{
      val geodesic = Geodesic(0.0+0.0*i, 0.5+0.5*i, SpaceType.PoincareDisc)
      val line = Line(0.0+0.0*i, 0.5+0.5*i)
      assert (geodesic.asCurve == line)
    }

    "returns a Line for z1=-(0.5+0.5i), z2=0.5+0.5i, in Poincare disc model"-{
      val geodesic = Geodesic(-0.5-0.5*i, 0.5+0.5*i, SpaceType.PoincareDisc)
      val line = Line(-0.5-0.5*i, 0.5+0.5*i)
      assert (geodesic.asCurve == line)
    }

  }
}
