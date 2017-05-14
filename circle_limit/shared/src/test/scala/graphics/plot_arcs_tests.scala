package circle_limit.graphics

import scala.math.{sqrt, pow}

import spire.math.Complex
import spire.implicits._
import utest._

import circle_limit.maths.Imaginary.i
import circle_limit.maths.{Arc, Circle, Line}


object ConverterTestSuite extends TestSuite {

  def square = pow(_: Double, 2)

  def d(w:Vector, z:Vector): Double = {
    sqrt(square(w.x - z.x) + square(w.y - z.y))
  }

  val tests = TestSuite {
    "Converts from mathematical to graphical correctly when:" - {
      "aspect-ratio of mathematical is less than graphical" - {
        val mathematicalBottomLeft = (-1.0, 0.0)
        val mathematicalWidth = 3.0
        val mathematicalHeight = 2.0
        val graphicalTopLeft = (0.0, 0.0)
        val graphicalWidth = 800
        val graphicalHeight = 400
        val converter = Converter(
          Box(mathematicalBottomLeft._1, mathematicalBottomLeft._2, mathematicalWidth, mathematicalHeight),
          Box(graphicalTopLeft._1, graphicalTopLeft._2, graphicalWidth, graphicalHeight))
        val f = converter.convertFromMathematicalToGraphicalSpace _
        val p1 = Complex(mathematicalBottomLeft._1, mathematicalBottomLeft._2)
        val p2 = Complex(mathematicalBottomLeft._1 + mathematicalWidth, mathematicalBottomLeft._2)
        val p3 = Complex(mathematicalBottomLeft._1, mathematicalBottomLeft._2 + mathematicalHeight)
        val p1Image = Vector(graphicalTopLeft._1 + 100, graphicalTopLeft._2 + graphicalHeight)
        val p2Image = Vector(graphicalTopLeft._1 + graphicalWidth - 100, graphicalTopLeft._2 + graphicalHeight)
        val p3Image = Vector(graphicalTopLeft._1 + 100, graphicalTopLeft._2)

        assert ( d(f(p1), p1Image) <= 0.0001 )
        assert ( d(f(p2), p2Image) <= 0.0001 )
        assert ( d(f(p3), p3Image) <= 0.0001 )
      }

      "aspect-ratio of mathematical is greater than graphical" - {
        val mathematicalBottomLeft = (-1.0, 0.0)
        val mathematicalWidth = 3.0
        val mathematicalHeight = 2.0
        val graphicalTopLeft = (0.0, 0.0)
        val graphicalWidth = 450
        val graphicalHeight = 500
        val converter = Converter(
          Box(mathematicalBottomLeft._1, mathematicalBottomLeft._2, mathematicalWidth, mathematicalHeight),
          Box(graphicalTopLeft._1, graphicalTopLeft._2, graphicalWidth, graphicalHeight))
        val f = converter.convertFromMathematicalToGraphicalSpace _
        val p1 = Complex(mathematicalBottomLeft._1, mathematicalBottomLeft._2)
        val p2 = Complex(mathematicalBottomLeft._1 + mathematicalWidth, mathematicalBottomLeft._2)
        val p3 = Complex(mathematicalBottomLeft._1, mathematicalBottomLeft._2 + mathematicalHeight)
        val p1Image = Vector(graphicalTopLeft._1, graphicalTopLeft._2 + graphicalHeight - 100)
        val p2Image = Vector(graphicalTopLeft._1 + graphicalWidth, graphicalTopLeft._2 + graphicalHeight - 100)
        val p3Image = Vector(graphicalTopLeft._1, graphicalTopLeft._2 + 100)

        assert ( d(f(p1), p1Image) <= 0.0001 )
        assert ( d(f(p2), p2Image) <= 0.0001 )
        assert ( d(f(p3), p3Image) <= 0.0001 )
      }
    }

    "Converts from graphical to mathematical correctly when:" - {
      "aspect-ratio of mathematical is less than graphical" - {
        val mathsBox = Box(-1.0, -1.0, 2.0, 2.0)
        val graphicsBox = Box(1.0, 2.0, 2.0, 1.0)
        val f = Converter(mathsBox, graphicsBox).convertFromGraphicalToMathematicalSpace _
        val p1 = Vector(graphicsBox.originX, graphicsBox.originY)
        val p2 = Vector(graphicsBox.originX+graphicsBox.width, graphicsBox.originY)
        val p3 = Vector(graphicsBox.originX, graphicsBox.originY+graphicsBox.height)
        val p1Image = Complex(-2.0, 1.0)
        val p2Image = Complex(2.0, 1.0)
        val p3Image = Complex(-2.0, -1.0)

        assert ( (f(p1)-p1Image).abs <= 0.0001 )
        assert ( (f(p2)-p2Image).abs <= 0.0001 )
        assert ( (f(p3)-p3Image).abs <= 0.0001 )
      }
      "aspect-ratio of mathematical is greater than graphical" - {
        val mathsBox = Box(-1.0, -1.0, 2.0, 2.0)
        val graphicsBox = Box(1.0, 2.0, 1.0, 4.0)
        val f = Converter(mathsBox, graphicsBox).convertFromGraphicalToMathematicalSpace _
        val p1 = Vector(graphicsBox.originX, graphicsBox.originY)
        val p2 = Vector(graphicsBox.originX+graphicsBox.width, graphicsBox.originY)
        val p3 = Vector(graphicsBox.originX, graphicsBox.originY+graphicsBox.height)
        val p1Image = Complex(-1.0, 4.0)
        val p2Image = Complex(1.0, 4.0)
        val p3Image = Complex(-1.0, -4.0)

        assert ( (f(p1)-p1Image).abs <= 0.0001 )
        assert ( (f(p2)-p2Image).abs <= 0.0001 )
        assert ( (f(p3)-p3Image).abs <= 0.0001 )
      }
    }

    "Converts SVG for an arc to Arc" - {
      val mathsBox = Box(-1.0, -1.0, 2.0, 2.0)
      val graphicsBox = Box(0.0, 0.0, 200.0, 200.0)
      val svg = "M 100, 0 A 100, 100, 0, 0, 0, 200, 100"
      val expectedArc = Arc(Complex(0,1), Complex(1,0), Complex(1,1))

      val returnedArc = Converter(mathsBox, graphicsBox).convertSvgToArc(svg)

      assert ((returnedArc.start-expectedArc.start).abs < 0.0001)
      assert ((returnedArc.finish-expectedArc.finish).abs < 0.0001)
      assert ((returnedArc.centre-expectedArc.centre).abs < 0.0001)
    }
    "Converts Arc to SVG for an arc" - {
      val mathsBox = Box(-1.0, -1.0, 2.0, 2.0)
      val graphicsBox = Box(0.0, 0.0, 200.0, 200.0)
      val arc = Arc(Complex(0,1), Complex(1,0), Complex(1,1))
      val expectedSvg = "M 100, 0 A 100, 100, 0, 0, 0, 200, 100"

      val returnedSvg = Converter(mathsBox, graphicsBox).convertArcToSvg(arc)

      assert (expectedSvg == returnedSvg)
    }

    "Converts SVG for a circle to a Circle" - {
      val mathsBox = Box(-1.0, -1.0, 2.0, 2.0)
      val graphicsBox = Box(0.0, 0.0, 200.0, 200.0)
      val (cx, cy, r) = ("100", "80", "20")
      val expectedCircle = Circle(Complex(0.0, 0.2), 0.2)

      val returnedCircle = Converter(mathsBox, graphicsBox).convertSvgToCircle(cx, cy, r)

      assert ((returnedCircle.centre-expectedCircle.centre).abs < 0.0001)
      assert (returnedCircle.radius-expectedCircle.radius < 0.0001)
    }
    "Converts Circle to SVG for a circle" - {
      val mathsBox = Box(-1.0, -1.0, 2.0, 2.0)
      val graphicsBox = Box(0.0, 0.0, 200.0, 200.0)
      val circle = Circle(Complex(0.0, 0.2), 0.2)
      val expected = ("100", "80", "20")

      val returned = Converter(mathsBox, graphicsBox).convertCircleToSvg(circle)

      assert (returned == expected)
    }
    
  }
}
