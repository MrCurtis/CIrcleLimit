package circle_limit.graphics

import scala.math.round

import spire.math.Complex
import spire.math.min
import spire.implicits._

import circle_limit.maths.{
  Arc,
  Circle,
  Curve,
  Line
}
import circle_limit.maths.DoubleMatrix
import circle_limit.maths.DoubleMatrix.DoubleMatrix


case class Vector(x: Double, y: Double)


/** 
 *  Used to represent a rectangle.
 */
case class Box(originX: Double, originY: Double, width: Double, height: Double)


/**
 * Methods for converting between the mathematical and graphical co-ordinate planes.
 *
 * We make use of 'boxes' - rectangles with edges parallel to the x and y axes - to define a
 * transformation between two planes.
 *
 * Given any two boxes - mathsBox and graphicsBox - the mapping is the unique Euclidean
 * transformation which satisfies the following:
 *   1 - The image of the mathematical rectangle is also a rectangle with the same aspect ratio
 *     as the original. 
 *   2 - The image of the mathematical rectangle takes up the maximum possible area in the 
 *     graphical rectangle.
 *     This means that:
 *       - If the aspect ratio of the graphical rectangle is wider than that of the mathematical
 *       rectangle then the tops of the graphical rectangle and the image of the mathematical
 *       rectangle will be so linear, and similarly with the bottom edges.
 *       - If the aspect ratio of the mathematical rectangle is wider than that of the graphical
 *       rectangle then the left edges of the graphical rectangle and the image of the
 *       mathematical rectangle will be co-linear, and similarly with the right edges.
 *  3 - The centres of the graphical rectangle and the image of the mathematical rectangles
 *    coincide.
 *  4 - The image of the mathematical rectangle is upside down. In other words the lower edge of
 *    the mathematical rectangle is mapped closer to the upper edge of the graphical rectangle
 *    than the lower edge of the graphical rectangle. This is to account for the fact that in a
 *    mathematical context the y-axis points upwards, whereas for in a graphics context - and
 *    particularly with regard to positioning on a web-page - the y-axis points downward.
 */
class Converter (mathsBox: Box, graphicsBox: Box){

  /**
   * Converts a Euclidean distance in the mathematical space to the corresponding distance in the
   * graphical space.
   */
  def scaleFromMathematicalToGraphicalSpace(distance: Double) = scaleFactor*distance

  /**
   * Converts a single point from mathematical space to graphical space.
   */
  def convertFromMathematicalToGraphicalSpace(point: Complex[Double]): Vector = {
    def transform(z: Complex[Double]) = scaleFactor*(z - centreOfMathsBox).conjugate + centreOfGraphicsBox
    convertComplexToVector( transform(point) )
  }

  /**
   * Converts a single point from graphical to mathematical space.
   */
  def convertFromGraphicalToMathematicalSpace(point: Vector): Complex[Double] = {
    val z = convertVectorToComplex(point)
    (z - centreOfGraphicsBox).conjugate/scaleFactor + centreOfMathsBox
  }

  /**
   * Takes the d attribute of an svg path element and converts it in to a Curve instance.
   */
  def convertSvgToCurve(svg: String): Curve = {
    val arcRegex = """^M (\d+) (\d+) A (\d+) (\d+) 0 0 0 (\d+) (\d+)$""".r
    val lineRegex = """^M (\d+) (\d+) L (\d+) (\d+)$""".r
    svg match {
      case arcRegex(initialX, initialY, radX, radY, finalX, finalY) => {
        Arc(
          startFromInitial(initialX, initialY),
          finishFromFinal(finalX, finalY),
          centreFromInitialFinalAndRadius(initialX, initialY, finalX, finalY, radX)
        )
      }
      case lineRegex(initialX, initialY, finalX, finalY) => {
        Line(
          startFromInitial(initialX, initialY),
          finishFromFinal(finalX, finalY)
        )
      }
    }
  }

  /**
   * Takes a Curve instance and creates the corresponding d attribute of the SVG path element.
   */
  def convertCurveToSvg(curve: Curve): String = {
    curve match{
      case arc: Arc => convertArcToSvg(arc)
      case line: Line => convertLineToSvg(line)
    }
  }

  /**
   * Takes the cx, cy, and r attributes of a circle SVG element and returns a Circle instance.
   */
  def convertSvgToCircle(cx: String, cy:String, r:String): Circle = {
    val centre = convertFromGraphicalToMathematicalSpace(Vector(cx.toDouble, cy.toDouble))
    val radius = radiusFromRadX(r)
    Circle(centre, radius)
  }

  /**
   * Takes a circle instance and returns the cx, cy, and r attributes of an SVG circle element.
   */
  def convertCircleToSvg(circle: Circle) = {
    val graphicalCentre = convertFromMathematicalToGraphicalSpace(circle.centre)
    val graphicalRadius = circle.radius * scaleFactor
    (round(graphicalCentre.x).toString, round(graphicalCentre.y).toString, round(graphicalRadius).toString)
  }

  private def convertArcToSvg(arc: Arc) = {
    val radX = (arc.centre-arc.start).abs * scaleFactor
    val initial = convertFromMathematicalToGraphicalSpace(arc.start)
    val finall = convertFromMathematicalToGraphicalSpace(arc.finish)
    "M %d %d A %d %d 0 0 0 %d %d".format(
      round(initial.x),
      round(initial.y),
      round(radX),
      round(radX),
      round(finall.x),
      round(finall.y))
  }

  private def convertLineToSvg(line: Line) = {
    val startGraphical = convertFromMathematicalToGraphicalSpace(line.start)
    val finishGraphical = convertFromMathematicalToGraphicalSpace(line.finish)
    "M %d %d L %d %d".format(
      round(startGraphical.x),
      round(startGraphical.y),
      round(finishGraphical.x),
      round(finishGraphical.y)
    )
  }

  private def convertComplexToVector(complex: Complex[Double]) = Vector(complex.real, complex.imag)

  private def convertVectorToComplex(vector: Vector) = Complex(vector.x, vector.y)

  private val centreOfMathsBox = Complex(
    mathsBox.originX+mathsBox.width/2,
    mathsBox.originY+mathsBox.height/2
  )

  private val centreOfGraphicsBox = Complex(
    graphicsBox.originX+graphicsBox.width/2,
    graphicsBox.originY+graphicsBox.height/2
  )

  private val scaleFactor = min(graphicsBox.width/mathsBox.width, graphicsBox.height/mathsBox.height)

  private def startFromInitial(initialX: String, initialY: String) =
    convertFromGraphicalToMathematicalSpace(Vector(initialX.toDouble, initialY.toDouble))

  private def finishFromFinal(finalX: String, finalY: String) =
    convertFromGraphicalToMathematicalSpace(Vector(finalX.toDouble, finalY.toDouble))

  private def radiusFromRadX(radX: String) = radX.toDouble / scaleFactor

  private def centreFromInitialFinalAndRadius(
      initialX: String,
      initialY: String,
      finalX: String,
      finalY: String,
      radX: String) = {
    val radius = radiusFromRadX(radX)
    val z = finishFromFinal(finalX, finalY) - startFromInitial(initialX, initialY)
    startFromInitial(initialX, initialY) + z/2.0*(1 + Complex(0.0, 1.0)*(4*radius*radius - z.abs*z.abs).sqrt/z.abs)
  }
}

object Converter {
  def apply(mathsBox: Box, graphicsBox: Box) = new Converter(mathsBox, graphicsBox)
}
