package circle_limit.graphics

import spire.math.Complex
import spire.implicits._

object ArcPlotter {

  /**
   * Maps a rectangle in the mathematical space to a rectangle in the graphical space.
   */
  def convertFromMathematicalToGraphicalSpace(
      mathematicalBottomLeft: (Double, Double),
      mathematicalWidth: Double,
      mathematicalHeight: Double,
      graphicalTopLeft: (Double, Double),
      graphicalWidth: Double,
      graphicalHeight: Double,
      point: (Double, Double)
  ): (Double, Double) = {
    def aspectRatio(width: Double, height: Double) = width/height
    def translateTopLeftToOrigin(z: Complex[Double]) = {
      val a = Complex(mathematicalBottomLeft._1, mathematicalBottomLeft._2)
      val b = Complex(0, mathematicalHeight)
      z - a - b
    }
    def scaleSoHeightIsGraphicalHeight(z: Complex[Double]) = {
      val k = graphicalHeight / mathematicalHeight
      k * z
    }
    def scaleSoWidthIsGraphicalWidth(z: Complex[Double]) = {
      val k = graphicalWidth / mathematicalWidth
      k * z
    }
    def reflectInXAxis(z: Complex[Double]) = Complex(z.real, -z.imag)
    def translateToBottomLeftOfGraphical(z: Complex[Double]) = {
      val w = Complex(graphicalTopLeft._1, graphicalTopLeft._2)
      z + w
    }
    def translateHorizontallyToCentreOfGraphical(z: Complex[Double]) = {
      val k = graphicalHeight / mathematicalHeight
      val x = Complex((graphicalWidth - k*mathematicalWidth)/2, 0.0)
      z + x
    }
    def translateVerticallyToCentreOfGraphical(z: Complex[Double]) = {
      val k = graphicalWidth / mathematicalWidth
      val x = Complex(0.0, (graphicalHeight - k*mathematicalHeight)/2)
      z + x
    }
    val transformWhenAspectOfMathematicalIsLessThanOrEqualToGraphical = {
      translateTopLeftToOrigin _ andThen 
      scaleSoHeightIsGraphicalHeight _ andThen 
      reflectInXAxis _ andThen 
      translateToBottomLeftOfGraphical _ andThen 
      translateHorizontallyToCentreOfGraphical _
    }
    val transformWhenAspectOfMathematicalIsGreaterThanGraphical = {
      translateTopLeftToOrigin _ andThen 
      scaleSoWidthIsGraphicalWidth _ andThen 
      reflectInXAxis _ andThen 
      translateToBottomLeftOfGraphical _ andThen 
      translateVerticallyToCentreOfGraphical _
    }

    val z = Complex(point._1, point._2)
    val resultAsComplex = if (
      aspectRatio(mathematicalWidth, mathematicalHeight) > aspectRatio(graphicalWidth, graphicalHeight)) {
        transformWhenAspectOfMathematicalIsGreaterThanGraphical(z)
      }else{
        transformWhenAspectOfMathematicalIsLessThanOrEqualToGraphical(z)
      }
    (resultAsComplex.real, resultAsComplex.imag)
  }
}
