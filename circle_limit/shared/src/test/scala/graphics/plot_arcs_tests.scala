package circle_limit.graphics

import scala.math.{sqrt, pow}

import spire.math.Complex
import spire.implicits._
import utest._

import circle_limit.maths.Imaginary.i
import circle_limit.maths.{Arc, Line}


object CreateConstructorStringFromListTestSuite extends TestSuite {

  def simpleTestTransform(z: Complex[Double]) = Vector(2*z.real, 2*z.imag)

  val tests = TestSuite {

    "returns a list of constructors seperated by semi-colon as string" - {
      val inputList = List(
        Arc(-1.0+0.0*i, 0.0-1.0*i, 0.0+0.0*i),
        Line(0.0+1.0*i, 1.0+0.0*i),
        Arc(1.0+1.0*i, -1.0+1.0*i, 0.0+1.0*i)
      )
      val expected = 
      "new Path().moveTo(-2.000000, 0.000000).arcTo(2.000000, 2.000000, 0, 0, 0, 0.000000, -2.000000)"+
      ".stroke('black', 2).addTo(stage);"+
      "new Path().moveTo(0.000000, 2.000000).lineTo(2.000000, 0.000000).stroke('black', 2).addTo(stage);"+
      "new Path().moveTo(2.000000, 2.000000).arcTo(2.000000, 2.000000, 0, 0, 0, -2.000000, 2.000000)"+
      ".stroke('black', 2).addTo(stage);"
      val returned = Converter.createConstructorStringFromList(
        inputList,
        simpleTestTransform _
      )
      assert (returned == expected)
    }
  }
}


object CreateConstructorCallStringTestSuite extends TestSuite {

  def simpleTestTransform(z: Complex[Double]) = Vector(2*z.real, 2*z.imag)

  val tests = TestSuite{

    "For Arc argument - returns a valid javascript Arc constructor call as string" - {
      val inputArc = Arc(-1.0+0.0*i, 0.0-1.0*i, 0.0+0.0*i)
      val returned = Converter.createConstructorCallString(
        inputArc,
        simpleTestTransform _
      )
      val expected =
        "new Path().moveTo(-2.000000, 0.000000).arcTo(2.000000, 2.000000, 0, 0, 0, 0.000000, -2.000000)"+
        ".stroke('black', 2).addTo(stage);"
      assert (returned == expected)
    }

    "For Line argument - returns a valid Path constructor with method call" - {
      val inputLine = Line(-1.0+0.0*i, 0.0-1.0*i)
      val returned = Converter.createConstructorCallString(
        inputLine,
        simpleTestTransform _
      )
      val expected =
        "new Path().moveTo(-2.000000, 0.000000).lineTo(0.000000, -2.000000).stroke('black', 2).addTo(stage);"
      assert (returned == expected)
      
    }

  }
}


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
  }
}
