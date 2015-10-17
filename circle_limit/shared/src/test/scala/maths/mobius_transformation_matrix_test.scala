package circle_limit

import utest._
import breeze.math._


object MobiusTransformationMatrixTestSuite extends TestSuite {
  val tests = TestSuite {

    "are equal if and only if their components are equal" - {
      val m1 = MoebiusTransformationMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val m2 = MoebiusTransformationMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val m3 = MoebiusTransformationMatrix(2.0+0.0*i, 2.0+0.0*i, 4.0+4.0*i, 4.0+4.0*i)
      val m4 = MoebiusTransformationMatrix(1.1+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      assert(m1 == m1)
      assert(m1 == m2)
      assert(m2 == m1)
      assert(m1 != m3)
      assert(m3 != m1)
      assert(m1 != m4)
      assert(m4 != m1)
    }

    "can be multiplied" - {
      val left = MoebiusTransformationMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val right = MoebiusTransformationMatrix(1.0+1.0*i, 0.0+0.0*i, 2.0-1.0*i, 1.0+3.0*i)
      val result = MoebiusTransformationMatrix(3.0+0.0*i, 1.0+3.0*i, 6.0+6.0*i, -4.0+8.0*i)
      assert (left * right == result)
    }

    "can be added" - {
      val left = MoebiusTransformationMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val right = MoebiusTransformationMatrix(1.0+1.0*i, 0.0+0.0*i, 2.0-1.0*i, 1.0+3.0*i)
      val result = MoebiusTransformationMatrix(2.0+1.0*i, 1.0+0.0*i, 4.0+1.0*i, 3.0+5.0*i)
      assert (left + right == result)
    }

    "can be subtracted" - {
      val left = MoebiusTransformationMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val right = MoebiusTransformationMatrix(1.0+1.0*i, 0.0+0.0*i, 2.0-1.0*i, 1.0+3.0*i)
      val result = MoebiusTransformationMatrix(0.0-1.0*i, 1.0+0.0*i, 0.0+3.0*i, 1.0-1.0*i)
      assert (left - right == result)
    }

    "det returns the correct determinant" - {
      val m1  = MoebiusTransformationMatrix(0.4+1.0*i, 23.0-0.1*i, -12.0+109.0*i, 0.0+1.0*i)
      val detOfm1 = 264.1-2507.7999999999997*i
      assert (m1.det == detOfm1)
    }

    "returnNormalizedToSL returns the canonical element of SL if matrix invertible" - {
      val m1  = MoebiusTransformationMatrix(0.4+1.0*i, 23.0-0.1*i, -12.0+109.0*i, 0.0+1.0*i)
      val sqrtOfDetOfm1 = 37.32136137188219-33.597381068330606*i
      val normalizedM1 = MoebiusTransformationMatrix(
        (0.4+1.0*i) / sqrtOfDetOfm1,
        (23.0-0.1*i) / sqrtOfDetOfm1,
        (-12.0+109.0*i) / sqrtOfDetOfm1,
        (0.0+1.0*i) / sqrtOfDetOfm1)
      //We consider difference to deal with precision errors
      val difference = m1.returnNormalizedToSL - normalizedM1
      val error_margin = 0.00000000001
      assert (difference.a.abs < error_margin)
      assert (difference.b.abs < error_margin)
      assert (difference.c.abs < error_margin)
      assert (difference.d.abs < error_margin)
    }

    "returnNormalizedToSL throws exception if matrix not invertible" - {
      val t = MoebiusTransformationMatrix(1.0+0.0*i, 1.0+0.0*i, 2.0+2.0*i, 2.0+2.0*i)
      val e = intercept[NonInvertibleMatrixException]{
        t.returnNormalizedToSL
      }
      val expected_error_message = "Cannot normalize matrix with determinant zero."
      assertMatch(e) {case NonInvertibleMatrixException(expected_error_message)=>}
    }
  
  }
}
