package circle_limit.maths

import utest._
import spire.math.{
  Complex
}
import spire.implicits._
import Imaginary.i

object LineTestSuite extends TestSuite {
  val tests = TestSuite{

    "values are accesible"-{
      val line = Line(1.0+2.0*i, 3.0+4.0*i)
      assert (line.start == 1.0+2.0*i)
      assert (line.finish == 3.0+4.0*i)
    }

    "two lines are equal if and only if the two end points are equal"-{
      val line1 = Line(1.0+2.0*i, 3.0+4.0*i)
      val line2 = Line(1.0+2.0*i, 3.0+4.0*i)
      val line3 = Line(3.0+4.0*i, 1.0+2.0*i)
      val line4 = Line(3.7+4.3*i, 8.0-2.0*i)
      val line5 = Line(1.0+2.0*i, 8.0-2.0*i)
      val line6 = Line(3.7+4.3*i, 3.0+4.0*i)

      assert (line1 == line1)
      assert (line1 == line2)
      assert (line2 == line1)
      assert (line1 == line3)
      assert (line3 == line1)
      assert (line1 != line4)
      assert (line4 != line1)
      assert (line1 != line5)
      assert (line5 != line1)
      assert (line1 != line6)
      assert (line6 != line1)
    }

    "line not equal to object of different type" - {
      val line = Line(1.0+2.0*i, 3.0+4.0*i)
      val arc = Arc(-1.0+0.0*i, 0.0+1.0*i, 0.0+0.0*i)

      assert (line != arc)
      assert (arc  != line)
    }

  }
}

object ArcTestSuite extends TestSuite{
  val tests = TestSuite{

    "values are accesible"-{
      val arc = Arc(1.0+2.0*i, 3.0+4.0*i, 5.0+6.0*i)
      assert (arc.start == 1.0+2.0*i)
      assert (arc.finish == 3.0+4.0*i)
      assert (arc.centre == 5.0+6.0*i)
    }

    "are equal only if centre and end points match"-{
      val arc1 = Arc(-1.0+0.0*i, 0.0+1.0*i, 0.0+0.0*i)
      val arc2 = Arc(-1.0+0.0*i, 0.0+1.0*i, 0.0+0.0*i)
      val arc3 = Arc(1.0+0.0*i, 0.0+1.0*i, 0.0+0.0*i)
      val arc4 = Arc(-1.0+0.0*i, 1.0+0.0*i, 0.0+0.0*i)
      val arc5 = Arc(-1.0+0.0*i, 0.0+1.0*i, -1.0+1.0*i)

      assert (arc1 == arc1)
      assert (arc1 == arc2)
      assert (arc2 == arc1)
      assert (arc1 != arc3)
      assert (arc3 != arc1)
      assert (arc1 != arc4)
      assert (arc4 != arc1)
      assert (arc1 != arc5)
      assert (arc5 != arc1)
    }

    "arc not equal to object of different type" - {
      val arc = Arc(-1.0+0.0*i, 0.0+1.0*i, 0.0+0.0*i)
      val circle = Circle(1.2+3.4*i, 5.6)

      assert (arc  != circle)
      assert (circle != arc)
    }

    "has obtuse / acute functionality"-{

        val acute = Arc(5.0+ 5.0*i, 4.0+6.0*i, 1.0+2.0*i)
        // We define obtuse to be the complement of acute.
        val obtuse = Arc(4.0+6.0*i, 5.0+ 5.0*i, 1.0+2.0*i) 
        val neither = Arc(5.0+5.0*i, -3.0-1.0*i, 1.0+2.0*i)

      "isAcute works properly"-{
        assert (acute.isAcute)
        assert (!(obtuse.isAcute))
        assert (!(neither.isAcute))
      }

      "isObtuse works properly"-{
        assert (!(acute.isObtuse))
        assert ((obtuse.isObtuse))
        assert (!(neither.isObtuse))
      }

      "complement works properly"-{
        val complement_of_neither = Arc(-3.0-1.0*i, 5.0+5.0*i, 1.0+2.0*i)

        assert (acute.complement == obtuse)
        assert (obtuse.complement == acute)
        assert (neither.complement == complement_of_neither)
      }

      "getAcute works properly"-{
        assert (acute.getAcute == acute)
        assert (obtuse.getAcute == acute)
        assert (neither.getAcute == neither)
      }

      "getObtuse works properly"-{
        assert (acute.getObtuse == obtuse)
        assert (obtuse.getObtuse == obtuse)
        assert (neither.getObtuse == neither)
      }

    }

  }
}


object CircleTestSuite extends TestSuite {
  val tests = TestSuite{

    "values are accesible"-{
      val circle = Circle(1.2+3.4*i, 5.6)
      assert (circle.centre == 1.2+3.4*i)
      assert (circle.radius == 5.6)
    }

    "two circles are equal if and only if their centres and radii are equal" - {
      val circle1 = Circle(1.2+3.4*i, 5.6)
      val circle2 = Circle(1.2+3.4*i, 5.6)
      val circle3 = Circle(1.2+3.2*i, 5.6)
      val circle4 = Circle(1.2+3.4*i, 5.8)

      assert (circle1 == circle1)
      assert (circle1 == circle2)
      assert (circle1 != circle3)
      assert (circle1 != circle4)
      assert (circle3 != circle4)
    }

    "circle not equal to object of different type" - {
      val circle = Circle(1.2+3.4*i, 5.6)
      val line = Line(1.2+3.4*i, 5.6)

      assert (circle != line)
      assert (line  != circle)
    }

  }
}
