package circle_limit.maths

import spire.math.{
  Complex
}
import spire.implicits._
import Imaginary.i

/**
 * A geodesic specified by its two end point z1 and z2, and the model
 * of hyperbolic space in which it is represented.
 */
class Geodesic(val z1: ProjectiveComplex, val z2: ProjectiveComplex, val spaceType: SpaceType.Value) {

  /**
   * Returns a more concrete representation of the geodesic, specific to 
   * the model in which it is to be plotted. Typically, this is what one 
   * would pass in to functions for plotting graphically.
   */
  def asCurve: Curve = spaceType match {
    case SpaceType.UpperHalfPlane => createCurveFromGeodesicInUpperHalfPlane(z1, z2)
    case SpaceType.PoincareDisc => createCurveFromGeodesicInPoincareDisc(z1, z2)
  }

  private def createCurveFromGeodesicInUpperHalfPlane(z1: ProjectiveComplex, z2: ProjectiveComplex): Curve = {

    (z1.toExtendedComplex, z2.toExtendedComplex) match {

      case (Right(w1: Complex[Double]), Right(w2: Complex[Double])) => 
        if (w1.real != w2.real){
          createArcFromComplexInUpperHalfPlane(w1, w2)
        } else {
          createLineFromComplex(w1, w2)
        }

      case (Right(_), Left(_)) | (Left(_), Right(_)) =>
        throw new NotImplementedError("Need to think about how to do this properly")

      case (Left(_), Left(_)) | (Left(_), Left(_)) =>
        throw new NotImplementedError("Make this impossible for a geodesic.")

      } 

  }

  private def createArcFromComplexInUpperHalfPlane(z1: Complex[Double], z2: Complex[Double]): Arc = {
    /*
    * A note on the calculations:
    *
    * If z_c is any point which is the centre of a circle which passes through both z_1 and z_2
    * then we have
    *   1) |z_1 - z_c| = |z_2 - z_c|
    * and thus
    *   2) (x_1 - x_c)^2 + (y_1 - y_c)^2 = (x_1 - x_c)^2 + (y_1 - y_c)^2
    * where x_2, y_2, x_1, y_1, and x_c, y_c are the real and imaginary parts of z_2, z_1, and
    * z_c respectively. If we further specify that z_c is is on the real line then we can
    * substitute in y_c = 0 in to (2) and rearrange to obtain the desired value of x_c.
    */
    def sq(x: Double) = x*x
    val zCentre =
      ( (sq(z2.real) + sq(z2.imag)) - (sq(z1.real) + sq(z1.imag)) ) / (2*(z2.real - z1.real))
    if (z1.real > z2.real) {
      Arc(z1, z2, zCentre)
    } else {
      Arc(z2, z1, zCentre)
    }
  }

  private def createLineFromComplex(z1: Complex[Double], z2: Complex[Double]): Line = {
    Line(z1, z2)
  }

  private def createCurveFromGeodesicInPoincareDisc(z1: ProjectiveComplex, z2: ProjectiveComplex): Curve = {

    (z1.toExtendedComplex, z2.toExtendedComplex) match {

      case (Right(w1: Complex[Double]), Right(w2: Complex[Double])) => 
        if (w1.real*w2.imag != w2.real*w1.imag) {
          createArcFromComplexInPoincareDisc(w1, w2)
        } else {
          createLineFromComplex(w1, w2)
        }

      case (Right(_), Left(_)) | (Left(_), Right(_)) =>
        throw new NotImplementedError("Infinity is not a point on the disc.")

      case (Left(_), Left(_)) | (Left(_), Left(_)) =>
        throw new NotImplementedError("Make this impossible for a geodesic.")

      } 

  }

  private def createArcFromComplexInPoincareDisc(z1: Complex[Double], z2: Complex[Double]): Arc = {
    /*
    * A note on the calculations:
    *
    * If z_c is any point outside of the closed unit disc, then the circle
    * centred at z_c which intersects the unit circle orthogonally has radius
    * r determined (using Pythagoras' theorem) by:
    *   1) r^2 = |z_c|^2 - 1
    * If, furthermore, this circle passes through both z_1 and z_2 then we
    * have:
    *   2) |z_c - z_1|^2 = r^2
    * and
    *   3) |z_c - z_2|^2 = r^2
    * Substituting the RHS of (1) for the RHS of (2) and rearranging we get
    *   4) x_c*x_1 + y_c*y_1 = (1/2) * (x_1^2 + y_1^2 + 1)
    * where x_c,y_c and x_1, y_1 are the real and imaginary parts of z_c and
    * z_1 respectively.
    * Similarly we have
    *   5) x_c*x_2 + y_c*y_2 = (1/2) * (x_2^2 + y_2^2 + 1)
    * Solving for x_c and y_c gives us the results below.
    */
    
    val x1 = z1.real
    val y1 = z1.imag
    val x2 = z2.real
    val y2 = z2.imag

    val a = 1 + x1*x1 + y1*y1
    val b = 1 + x2*x2 + y2*y2
    val c = 2*(x1*y2 - x2*y1)
    val xC = (a*y2 - b*y1)/c
    val yC = (b*x1 - a*x2)/c

    val zC = xC + yC*i

    Arc(z1, z2, zC).getAcute

  }

  // Must be symmetrical with respect to z1 and z2
  override def hashCode: Int = z1.hashCode + z2.hashCode

  def canEqual(other: Any) = other.isInstanceOf[Geodesic]

  /**
   * Geodesics are equal if and only if their end points are equal 
   * (regardless of order) and they have the same SpaceType.
   */
  override def equals (that: Any) = that match {
    case that: Geodesic => 
      (that canEqual this) &&
      ( (this.z1 == that.z1 && this.z2 == that.z2) || (this.z1 == that.z2 && this.z2 == that.z1) ) &&
      (this.spaceType == that.spaceType)
    case _ => false
  }
  
}
object Geodesic {
  def apply(z1: ProjectiveComplex, z2: ProjectiveComplex, spaceType: SpaceType.Value) = 
    new Geodesic(z1, z2, spaceType)
}
