package circle_limit.maths

import utest._

import spire.math.{
  Complex
}
import spire.implicits._
import Imaginary.i


object MobiusTransformationTestSuite extends TestSuite{
  val tests = TestSuite{

    "attempting to create an non-invertible transformation raises exception"-{
      val e = intercept[NonInvertibleMatrixException]{
        val t = MoebiusTransformation(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      }
      assertMatch(e) {case NonInvertibleMatrixException(
        "Attempting to create a non-invertible transformation.")=>
      }
    }

    "applying a transformation to a complex number returns the correct result"-{
      import CircleImplicits.complexToProjectiveComplex
      val t = MoebiusTransformation(1.0+0.0*i, 1.0+0.0*i, 0.0+0.0*i, 1.0+0.0*i)
      val z = 1.0 + 1.0*i
      val w = t transform z
      assert (w.toExtendedComplex == Right(2.0 + 1.0*i))
    }

    "applying a transformation to a geodesic returns the correct geodesic"-{
      import CircleImplicits.complexToProjectiveComplex
      val t = MoebiusTransformation(0.0+1.0*i, 0.0+0.0*i, 0.0+0.0*i, 1.0+0.0*i)
      //TODO - Work out why type conversion doesn't work here
      val geod = Geodesic(Complex[Double](0.4, 0.2), Complex[Double](0.8, 0.4), SpaceType.PoincareDisc)
      val expected = Geodesic(Complex[Double](-0.2, 0.4), Complex[Double](-0.4, 0.8), SpaceType.PoincareDisc)
      val returned = t transform geod
      assert (returned == expected)
    }

    "almostEquality is determined as an element of PSL(2,C)"-{
      val t1 = MoebiusTransformation(1.0+0.0*i, 2.0-2.0*i, 5.0+0.0*i, 7.0+0.0*i)
      val t2 = MoebiusTransformation(2.0+0.0*i, 4.0-4.0*i, 10.0+0.0*i, 14.0+0.0*i)
      val t3 = MoebiusTransformation(-1.0+0.0*i, -2.0+2.0*i, -5.0+0.0*i, -7.0+0.0*i)
      assert (t1 almostEquals t2)
      assert (t1 almostEquals t3)
    }

    "transformations which are 'close' norm-wise are treated as 'almost' equal - to ignore rounding"-{
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
      assert (t1 almostEquals t2)
    }

    "composing two transformations results in the correct transformation"-{
      val t1 = MoebiusTransformation(1.0+0.0*i, 2.0-2.0*i, 5.0+0.0*i, 7.0+0.0*i)
      val t2 = MoebiusTransformation(-1.0+0.0*i, 1.0+0.0*i, 0.0+0.0*i, 1.0*i)
      val expected = MoebiusTransformation(-1.0+0.0*i, 3.0+2.0*i, -5.0+0.0*i, 5.0 + 7.0*i)
      val composed = t1 compose t2
      assert (composed almostEquals expected)
    }

  }
}
