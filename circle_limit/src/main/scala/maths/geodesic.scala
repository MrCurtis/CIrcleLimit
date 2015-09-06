package circle_limit

import breeze.math.{
  Complex, 
  i
}

/**
 * A geodesic specified by its two end point z1 and z2, and the model
 * of hyperbolic space in which it is represented.
 */
class Geodesic(z1: ProjectiveComplex, z2: ProjectiveComplex, spaceType: SpaceType.Value) {

  /**
   * Returns a more concrete representation of the geodesic, specific to 
   * the model in which it is to be plotted. Typically, this is what one 
   * would pass in to functions for plotting graphically.
   */
  def asCurve: Curve = spaceType match {
    case SpaceType.UpperHalfPlane => createCurveFromGeodesicInUpperHalfPlane(z1, z2)
  }

  private def createCurveFromGeodesicInUpperHalfPlane(z1: ProjectiveComplex, z2: ProjectiveComplex): Curve = {

    (z1.toExtendedComplex, z2.toExtendedComplex) match {

      case (Right(w1: Complex), Right(w2: Complex)) => 
        if (w1.real == w2.real){
          createLineFromComplexInUpperHalfPlane(w1, w2)
        }else{
          createArcFromComplexInUpperHalfPlane(w1, w2)
        }

      case (Right(_), Left(_)) | (Left(_), Right(_)) =>
        throw new NotImplementedError("Need to think about how to do this properly")

      case (Left(_), Left(_)) | (Left(_), Left(_)) =>
        throw new NotImplementedError("Make this impossible for a geodesic.")

      } 

  }

  private def createArcFromComplexInUpperHalfPlane(z1: Complex, z2: Complex): Arc = {
    //TODO - Explain the maths for calculating the centre.
    val k = (z2.imag + z1.imag) / (2 * (z2.real - z1.real)) 
    val zCentre = (z2 * (1 -2*k*i) + z1 * (1 + 2*k*i))/2.0+0.0*i
    if (z1.real > z2.real) {
      Arc(z1, z2, zCentre)
    }else{
      Arc(z2, z1, zCentre)
    }
  }

  private def createLineFromComplexInUpperHalfPlane(z1: Complex, z2: Complex): Line = {
    Line(1.0+5.0*i, 1.0+7.0*i)
  }
  
}
object Geodesic {
  def apply(z1: ProjectiveComplex, z2: ProjectiveComplex, spaceType: SpaceType.Value) = 
    new Geodesic(z1, z2, spaceType)
}
