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
    
    "allWordsOfLength(3) returns correct elements for a torsion-free group" - {
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

      val group = Group(List(transform1, transform2))
      val returned = group.allWordsOfLength(3)

      println ("Debugging stuff: ")
      //print ("expected size: ")
      //print (expected.size)
      //print ("expected: ")
      //print (expected.toString)
      //println ("")
      //print ("returned: ")
      //print (returned.toString)
      print ("returned size: ")
      print (returned.size)

      assert ( checkSetsOfMoebiusTransformationAlmostEqual ( expected, returned ) )
    } 
  }
}
