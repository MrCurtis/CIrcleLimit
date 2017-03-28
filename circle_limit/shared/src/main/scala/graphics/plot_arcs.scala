package circle_limit.graphics

import spire.math.Complex
import spire.math.min
import spire.implicits._

import circle_limit.maths.{
  Arc,
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


class Converter (mathsBox: Box, graphicsBox: Box){

  /**
   * We make use of 'boxes' - rectangles with edges parralel to the x and y axes - to define a
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

  /*
   * Converts a single point from mathematical space to graphical space.
   */
  def convertFromMathematicalToGraphicalSpace(point: Complex[Double]): Vector = {
    def transform(z: Complex[Double]) = scaleFactor*(z - centreOfMathsBox).conjugate + centreOfGraphicsBox
    convertComplexToVector( transform(point) )
  }

  /*
   * Converts a single point from graphical to mathematical space.
   */
  def convertFromGraphicalToMathematicalSpace(point: Vector): Complex[Double] = {
    val z = convertVectorToComplex(point)
    (z - centreOfGraphicsBox).conjugate/scaleFactor + centreOfMathsBox
  }
}

object Converter {
  def apply(mathsBox: Box, graphicsBox: Box) = new Converter(mathsBox, graphicsBox)

  /**
   * DEPRECATED
   * Creates a string calling the JS constructor on each of the curves.
   *
   * The coordinates are transformed by the transform function before being
   * formatted into the string.
   */
  def createConstructorStringFromList(
    listOfArcs: List[Curve],
    transform: Function[Complex[Double], Vector]
  ): String = {
    val stringForEachCurve = createConstructorCallString(_: Curve, transform)
    listOfArcs.map(stringForEachCurve).mkString
  }

  /**
   * DEPRECATED
   * Creates a string representation of a bonsai contructor call.
   *
   * The coordinates are transformed by the transform function before being
   * formatted into the string.
   */
  def createConstructorCallString(curve: Curve, transform: Function[Complex[Double], Vector]): String = {
    def transformComplex(complex: Complex[Double]) = {
      val transformedVector = transform(complex)
      Complex[Double](transformedVector.x, transformedVector.y)
    }
    curve match {
      case arc: Arc => {
        val transformedCentre = transformComplex(arc.centre)
        val transformedStart = transformComplex(arc.start)
        val transformedFinish = transformComplex(arc.finish)
        val x = transformedCentre.real
        val y = transformedCentre.imag
        val radius = (transformedStart - transformedCentre).abs
        val startAngle = (transformedStart - transformedCentre).arg
        val endAngle = (transformedFinish - transformedCentre).arg
        val startX = transformedStart.real
        val startY = transformedStart.imag
        val finishX = transformedFinish.real
        val finishY = transformedFinish.imag
        val string = "new Path().moveTo(%f, %f).arcTo(%f, %f, 0, 0, 0, %f, %f).stroke('black', 2).addTo(stage);".format(startX, startY, radius, radius, finishX, finishY)
        string
      }
      case line: Line => {
        val transformedStart = transformComplex(line.start)
        val transformedFinish = transformComplex(line.finish)
        "new Path().moveTo(%f, %f).lineTo(%f, %f).stroke('black', 2).addTo(stage);".format(
          transformedStart.real, transformedStart.imag, transformedFinish.real, transformedFinish.imag
        )
      }
    }
  }
}
