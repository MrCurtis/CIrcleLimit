package circle_limit.maths

import utest._

import spire.math.{
  Complex
}
import spire.implicits._
import Imaginary.i


object MobiusTransformationMatrixTestSuite extends TestSuite {
  val tests = TestSuite {

    "are equal if and only if their components are equal" - {
      val m1 = ComplexMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val m2 = ComplexMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val m3 = ComplexMatrix(2.0+0.0*i, 2.0+0.0*i, 4.0+4.0*i, 4.0+4.0*i)
      val m4 = ComplexMatrix(1.1+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      assert(m1 == m1)
      assert(m1 == m2)
      assert(m2 == m1)
      assert(m1 != m3)
      assert(m3 != m1)
      assert(m1 != m4)
      assert(m4 != m1)
    }

    "can be multiplied" - {
      val left = ComplexMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val right = ComplexMatrix(1.0+1.0*i, 0.0+0.0*i, 2.0-1.0*i, 1.0+3.0*i)
      val result = ComplexMatrix(3.0+0.0*i, 1.0+3.0*i, 6.0+6.0*i, -4.0+8.0*i)
      assert (left * right == result)
    }

    "can be added" - {
      val left = ComplexMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val right = ComplexMatrix(1.0+1.0*i, 0.0+0.0*i, 2.0-1.0*i, 1.0+3.0*i)
      val result = ComplexMatrix(2.0+1.0*i, 1.0+0.0*i, 4.0+1.0*i, 3.0+5.0*i)
      assert (left + right == result)
    }

    "can be subtracted" - {
      val left = ComplexMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val right = ComplexMatrix(1.0+1.0*i, 0.0+0.0*i, 2.0-1.0*i, 1.0+3.0*i)
      val result = ComplexMatrix(0.0-1.0*i, 1.0+0.0*i, 0.0+3.0*i, 1.0-1.0*i)
      assert (left - right == result)
    }

    "det returns the correct determinant" - {
      val m1  = ComplexMatrix(0.4+1.0*i, 23.0-0.1*i, -12.0+109.0*i, 0.0+1.0*i)
      val detOfm1 = 264.1-2507.7999999999997*i
      assert (m1.det == detOfm1)
    }

    "has an identity element" - {
      val expected = ComplexMatrix(1.0+0.0*i, 0.0 +0.0*i, 0.0+0.0*i, 1.0+0.0*i)

      val returned = ComplexMatrix.identity

      assert (expected equals returned)
    }

    "each element has an inverse" - {
      val matrix = ComplexMatrix(
        2.0+0.0*i, 0.0+1.0*i,
        0.0+4.0*i, 0.0+2.0*i
      )

      assert (matrix * matrix.inverse equals ComplexMatrix.identity)
    }
  
  }
}

object DoubleMatrixTestSuite extends TestSuite {
  val tests = TestSuite {

    "are equal if and only if their components are equal" - {
      val m1 = DoubleMatrix(1.0, 1.0, 2.0, 2.0)
      val m2 = DoubleMatrix(1.0, 1.0, 2.0, 2.0)
      val m3 = DoubleMatrix(2.0, 2.0, 4.0, 4.0)
      val m4 = DoubleMatrix(1.1, 1.0, 2.0, 2.0)
      assert(m1 == m1)
      assert(m1 == m2)
      assert(m2 == m1)
      assert(m1 != m3)
      assert(m3 != m1)
      assert(m1 != m4)
      assert(m4 != m1)
    }

    "can be multiplied" - {
      val left = DoubleMatrix(1.0, 1.0, 2.0, 2.0)
      val right = DoubleMatrix(1.0, 0.0, 2.0, 1.0)
      val result = DoubleMatrix(3.0, 1.0, 6.0, 2.0)
      assert (left * right == result)
    }

    "can be added" - {
      val left = DoubleMatrix(1.0, 1.0, 2.0, 2.0)
      val right = DoubleMatrix(1.0, 0.0, 2.0, 1.0)
      val result = DoubleMatrix(2.0, 1.0, 4.0, 3.0)
      assert (left + right == result)
    }

    "can be subtracted" - {
      val left = DoubleMatrix(1.0, 1.0, 2.0, 2.0)
      val right = DoubleMatrix(1.0, 0.0, 2.0, 1.0)
      val result = DoubleMatrix(0.0, 1.0, 0.0, 1.0)
      assert (left - right == result)
    }

    "det returns the correct determinant" - {
      val m1  = DoubleMatrix(0.4, 23.0, -12.0, 0.1)
      val detOfm1 = 276.04
      assert (m1.det == detOfm1)
    }

    "has an identity element" - {
      val expected = DoubleMatrix(1.0, 0.0, 0.0, 1.0)

      val returned = DoubleMatrix.identity

      assert (expected equals returned)
    }

    "elements have an inverse" - {
      val matrix = DoubleMatrix(
        2.0, 3.0,
        0.0, 1.0
      )

      assert (matrix * matrix.inverse equals DoubleMatrix.identity)
    }
  
  }
}
