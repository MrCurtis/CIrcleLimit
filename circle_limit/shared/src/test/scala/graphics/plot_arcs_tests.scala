package circle_limit.graphics

import scala.math.{sqrt, pow}

import spire.math.Complex
import spire.implicits._
import utest._

import circle_limit.maths.Imaginary.i
import circle_limit.maths.Arc


object CreateConstructorCallStringTestSuite extends TestSuite {

  def simpleTestTransform(z: (Double, Double)) = (2*z._1, 2*z._2)

  val tests = TestSuite{

    "returns a valid javascript Arc constructor call as string" - {
      val inputArc = Arc(-1.0+0.0*i, 0.0-1.0*i, 0.0+0.0*i)
      val returned = ArcPlotter.createConstructorCallString(
        inputArc,
        simpleTestTransform _
      )
      val expected =
        "new Arc(0.000000, 0.000000, 2.000000, 3.141593, -1.570796, true).addTo(stage);"
      assert (returned == expected)
    }

  }
}


object ConvertFromMathematicalToGraphicalSpaceTestSuite extends TestSuite {

  def square = pow(_: Double, 2)

  def d(w:(Double, Double), z:(Double, Double)): Double = {
    sqrt(square(w._1 - z._1) + square(w._2 - z._2))
  }

  val tests = TestSuite {

    "When aspect-ratio of mathematical is less than graphical:" - {
      val mathematicalBottomLeft = (-1.0, 0.0)
      val mathematicalWidth = 3.0
      val mathematicalHeight = 2.0
      val graphicalTopLeft = (0.0, 0.0)
      val graphicalWidth = 800
      val graphicalHeight = 400
      val f = ArcPlotter.convertFromMathematicalToGraphicalSpace(
        mathematicalBottomLeft, 
        mathematicalWidth, 
        mathematicalHeight, 
        graphicalTopLeft, 
        graphicalWidth, 
        graphicalHeight, 
        _: (Double, Double)
      ) 
      val p1 = (mathematicalBottomLeft._1, mathematicalBottomLeft._2)
      val p2 = (mathematicalBottomLeft._1 + mathematicalWidth, mathematicalBottomLeft._2)
      val p3 = (mathematicalBottomLeft._1, mathematicalBottomLeft._2 + mathematicalHeight)
      val p1Image = (graphicalTopLeft._1 + 100, graphicalTopLeft._2 + graphicalHeight)
      val p2Image = (graphicalTopLeft._1 + graphicalWidth - 100, graphicalTopLeft._2 + graphicalHeight)
      val p3Image = (graphicalTopLeft._1 + 100, graphicalTopLeft._2)

      assert ( d(f(p1), p1Image) <= 0.0001 )
      assert ( d(f(p2), p2Image) <= 0.0001 )
      assert ( d(f(p3), p3Image) <= 0.0001 )
    }

    "When aspect-ratio of mathematical is greater than graphical:" - {
      val mathematicalBottomLeft = (-1.0, 0.0)
      val mathematicalWidth = 3.0
      val mathematicalHeight = 2.0
      val graphicalTopLeft = (0.0, 0.0)
      val graphicalWidth = 450
      val graphicalHeight = 500
      val f = ArcPlotter.convertFromMathematicalToGraphicalSpace(
        mathematicalBottomLeft, 
        mathematicalWidth, 
        mathematicalHeight, 
        graphicalTopLeft, 
        graphicalWidth, 
        graphicalHeight, 
        _: (Double, Double)
      ) 
      val p1 = (mathematicalBottomLeft._1, mathematicalBottomLeft._2)
      val p2 = (mathematicalBottomLeft._1 + mathematicalWidth, mathematicalBottomLeft._2)
      val p3 = (mathematicalBottomLeft._1, mathematicalBottomLeft._2 + mathematicalHeight)
      val p1Image = (graphicalTopLeft._1, graphicalTopLeft._2 + graphicalHeight - 100)
      val p2Image = (graphicalTopLeft._1 + graphicalWidth, graphicalTopLeft._2 + graphicalHeight - 100)
      val p3Image = (graphicalTopLeft._1, graphicalTopLeft._2 + 100)

      assert ( d(f(p1), p1Image) <= 0.0001 )
      assert ( d(f(p2), p2Image) <= 0.0001 )
      assert ( d(f(p3), p3Image) <= 0.0001 )
    }

  }
}
