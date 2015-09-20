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

    "has well-defined equality operator:"-{
        val geod1 = Geodesic(0.0+5.0*i, 3.0+4.0*i, SpaceType.UpperHalfPlane)
        val geod2 = Geodesic(0.0+5.0*i, 3.0+4.0*i, SpaceType.UpperHalfPlane)
        val geod3 = Geodesic(3.0+4.0*i, 0.0+5.0*i, SpaceType.UpperHalfPlane)
        val geod4 = Geodesic(0.0+5.0*i, 3.0+4.0*i, SpaceType.PoincareDisc)
        val geod5 = Geodesic(3.0+4.0*i, 0.0+5.0*i, SpaceType.PoincareDisc)
        val geod6 = Geodesic(1.5+5.0*i, 3.7+4.0*i, SpaceType.UpperHalfPlane)

      "instances are equal only if have same end points and same space type"-{
        assert (geod1 == geod1)
        assert (geod2 == geod1)
        assert (geod1 == geod2)
        assert (geod1 == geod3)
        assert (geod3 == geod1)
        assert (geod1 != geod4)
        assert (geod4 != geod1)
        assert (geod1 != geod5)
        assert (geod5 != geod1)
        assert (geod1 != geod6)
        assert (geod6 != geod1)
      }

      "equality implies hashCode equality"-{
        val geodesics = List(geod1, geod2, geod3, geod4, geod5, geod6)
        for (p <- geodesics; q <- geodesics) 
          assert ( (p != q) || (p.hashCode == q.hashCode) )
      }

    }

  }
}
