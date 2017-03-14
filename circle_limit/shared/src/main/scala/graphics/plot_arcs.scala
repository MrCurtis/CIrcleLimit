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


object ArcPlotter {

  /**
   * Maps a rectangle in the mathematical space to into a rectangle in the graphical space.
   *
   * The mathematical rectangle in to the graphical rectangle in such a way that:
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
   *
   *  NOTE - This function is the inverse of convertFromGraphicalToMathematicalSpace.
   */
  def convertFromMathematicalToGraphicalSpace(
      mathematicalBottomLeft: (Double, Double),
      mathematicalWidth: Double,
      mathematicalHeight: Double,
      graphicalTopLeft: (Double, Double),
      graphicalWidth: Double,
      graphicalHeight: Double,
      point: Complex[Double]
  ): Vector = {
    def convertComplexToVector(complex: Complex[Double]) = Vector(complex.real, complex.imag)
    def convertVectorToComplex(vector: (Double, Double)) = Complex(vector._1, vector._2)
    val centreOfMathsBox = Complex(
      mathematicalBottomLeft._1+mathematicalWidth/2,
      mathematicalBottomLeft._2+mathematicalHeight/2
    )
    val centreOfGraphicsBox = Complex(
      graphicalTopLeft._1+graphicalWidth/2,
      graphicalTopLeft._2+graphicalHeight/2
    )
    val scaleFactor = min(graphicalWidth/mathematicalWidth, graphicalHeight/mathematicalHeight)
    def transform(z: Complex[Double]) = scaleFactor*(z - centreOfMathsBox).conjugate + centreOfGraphicsBox

    convertComplexToVector( transform(point) )
  }

  /**
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
