package circle_limit.maths

import utest._

import spire.math.{
  Complex
}
import spire.implicits._
import Imaginary.i

import TestHelpers.checkSetsOfMoebiusTransformationAlmostEqual

object GroupTestSuite extends TestSuite {

  val tests = TestSuite {
    
    "with wordLength=3, elements returns correct elements for a torsion-free group" - {
      val identity = MoebiusTransformation(1.0, 0.0, 0.0, 1.0)
      val transform1 = MoebiusTransformation(3.0, 2.0+1.0*i, 2.0-1.0*i, 3.0)
      val transform1Inverse = MoebiusTransformation(3.0, -2.0-1.0*i, -2.0+1.0*i, 3.0)
      val transform2 = MoebiusTransformation(3.0, 2.0-1.0*i, 2.0+1.0*i, 3.0)
      val transform2Inverse = MoebiusTransformation(3.0, -2.0+1.0*i, -2.0-1.0*i, 3.0)

      val expected = Set(
        identity,
        transform1,
        transform1Inverse,
        transform2,
        transform2Inverse,

        transform1 compose transform1,
        transform1 compose transform2,
        transform1 compose transform2Inverse,
        transform1Inverse compose transform1Inverse,
        transform1Inverse compose transform2,
        transform1Inverse compose transform2Inverse,
        transform2 compose transform2,
        transform2 compose transform1,
        transform2 compose transform1Inverse,
        transform2Inverse compose transform2Inverse,
        transform2Inverse compose transform1,
        transform2Inverse compose transform1Inverse,

        transform1 compose transform1 compose transform1,
        transform1 compose transform1 compose transform2,
        transform1 compose transform1 compose transform2Inverse,
        transform1 compose transform2 compose transform2,
        transform1 compose transform2 compose transform1,
        transform1 compose transform2 compose transform1Inverse,
        transform1 compose transform2Inverse compose transform2Inverse,
        transform1 compose transform2Inverse compose transform1,
        transform1 compose transform2Inverse compose transform1Inverse,

        transform1Inverse compose transform1Inverse compose transform1Inverse,
        transform1Inverse compose transform1Inverse compose transform2,
        transform1Inverse compose transform1Inverse compose transform2Inverse,
        transform1Inverse compose transform2 compose transform2,
        transform1Inverse compose transform2 compose transform1,
        transform1Inverse compose transform2 compose transform1Inverse,
        transform1Inverse compose transform2Inverse compose transform2Inverse,
        transform1Inverse compose transform2Inverse compose transform1,
        transform1Inverse compose transform2Inverse compose transform1Inverse,

        transform2 compose transform1 compose transform1,
        transform2 compose transform1 compose transform2,
        transform2 compose transform1 compose transform2Inverse,
        transform2 compose transform1Inverse compose transform1Inverse,
        transform2 compose transform1Inverse compose transform2,
        transform2 compose transform1Inverse compose transform2Inverse,
        transform2 compose transform2 compose transform2,
        transform2 compose transform2 compose transform1,
        transform2 compose transform2 compose transform1Inverse,

        transform2Inverse compose transform1 compose transform1,
        transform2Inverse compose transform1 compose transform2,
        transform2Inverse compose transform1 compose transform2Inverse,
        transform2Inverse compose transform1Inverse compose transform1Inverse,
        transform2Inverse compose transform1Inverse compose transform2,
        transform2Inverse compose transform1Inverse compose transform2Inverse,
        transform2Inverse compose transform2Inverse compose transform2Inverse,
        transform2Inverse compose transform2Inverse compose transform1,
        transform2Inverse compose transform2Inverse compose transform1Inverse
      )

      val wordLength = 3
      val group = Group(List(transform1, transform2), wordLength)
      val returned = group.elements

      assert ( checkSetsOfMoebiusTransformationAlmostEqual ( expected, returned ) )
    } 
  }
}
