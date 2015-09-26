package circle_limit

import utest._
import breeze.math._

object MobiusTransformationTestSuite extends TestSuite{
  val tests = TestSuite{

    "attempting to create an non-invertible transformation raises exception"-{
      val e = intercept[NonInvertibleMatrixException]{
        val t = MoebiusTransformation(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      }
      val expected_error_message = "Attempting to create a non-invertible transformation"
      assertMatch(e) {case NonInvertibleMatrixException(expected_error_message)=>}
    }

    "applying a transformation to a complex number returns the correct result"-{
      import CircleImplicits._
      val t = MoebiusTransformation(1.0+0*i, 1.0+0*i, 0.0+0*i, 1.0+0*i)
      val z = 1.0 + 1.0*i
      val w = t transform z
      assert (w.toExtendedComplex == Right(2.0 + 1.0*i))
    }

    "applying a transformation to a geodesic returns the correct geodesic"-{
      import CircleImplicits._
      val t = MoebiusTransformation(0.0+1.0*i, 0.0+0.0*i, 0.0+0.0*i, 1.0+0.0*i)
      val geod = Geodesic(0.4+0.2*i, 0.8+0.4*i, SpaceType.PoincareDisc)
      val expected = Geodesic(-0.2+0.4*i, -0.4+0.8*i, SpaceType.PoincareDisc)
      val returned = t transform geod
      assert (returned == expected)
    }

    "equality is determined as an element of PSL(2,C)"-{
      val t1 = MoebiusTransformation(1.0+0.0*i, 2.0-2.0*i, 5.0+0.0*i, 7.0+0.0*i)
      val t2 = MoebiusTransformation(2.0+0.0*i, 4.0-4.0*i, 10.0+0.0*i, 14.0+0.0*i)
      val t3 = MoebiusTransformation(-1.0+0.0*i, -2.0+2.0*i, -5.0+0.0*i, -7.0+0.0*i)
      assert (t1 equal t2)
      assert (t1 equal t3)
    }

    "transformations which are 'close' norm-wise are treated as equal - to ignore rounding"-{
      val t1 = MoebiusTransformation(
        -0.3062072401072272 + 0.04494168029824927*i,
        1.0085050809181804 + 0.4775894393197067*i,
        -1.5310362005361362 + 0.22470840149124635*i,
        1.845627962623881 + 1.9187422792593443*i
      )
      val t2 = MoebiusTransformation(
        -0.3062072401072272 + 0.04494168029824924*i,
        1.0085050809181801 + 0.4775894393197067*i,  
        -1.531036200536136 + 0.22470840149124627*i,
        1.8456279626238805 + 1.9187422792593445*i  
      )
      assert (t1 equal t2)
    }

    "composing two transformations results in the correct transformation"-{
      val t1 = MoebiusTransformation(1.0+0.0*i, 2.0-2.0*i, 5.0+0.0*i, 7.0+0.0*i)
      val t2 = MoebiusTransformation(-1.0+0.0*i, 1.0+0.0*i, 0.0+0.0*i, 1.0*i)
      val expected = MoebiusTransformation(-1.0+0.0*i, 3.0+2.0*i, -5.0+0.0*i, 5.0 + 7.0*i)
      val composed = t1 compose t2
      assert (composed equal expected)
    }

  }
}
