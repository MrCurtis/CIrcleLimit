package circle_limit.maths

import spire.math.{
  Complex
}
import spire.implicits._

import Imaginary.i
import ComplexMatrix.ComplexMatrix

/**
 * A Moebius transformation of form az+b/cz+d.
 */
class MoebiusTransformation(a: Complex[Double], b: Complex[Double], c: Complex[Double], d: Complex[Double]) {


  /**
   * If mt is a MobiusTransformation and z a point of the Reimann sphere, then 
   * mt transform z returns the image of z under mt.
   */
  def transform(projectiveComplex: ProjectiveComplex): ProjectiveComplex = 
    new ProjectiveComplex (
      (a*projectiveComplex.z + b*projectiveComplex.w,
      c*projectiveComplex.z + d*projectiveComplex.w)
    )

  /**
   * Returns the image of the geodisec under the Moebius Transformation.
   *
   * At present we consider each transformation to be a conformal
   * isomorphism of a particular hyperbolic space. Thus the image has the
   * same space type as the original geodesic. This might change in the 
   * future.
   */
  def transform(geodesic: Geodesic): Geodesic = 
    Geodesic (
      transform(geodesic.z1),
      transform(geodesic.z2),
      geodesic.spaceType
    )

  /**
   * Returns the composite of the two transformations.
   */
  def compose(that: MoebiusTransformation) = {
    val composedMatrix = this.theTransformationMatrix * that.theTransformationMatrix
    new MoebiusTransformation(
      composedMatrix.a,
      composedMatrix.b,
      composedMatrix.c,
      composedMatrix.d
    )
  }

  /**
   * If m is a matrix in GL, the general linear group, then m.returnNormalizedToSL
   * returns the canonical element in the equivalence class of SL, the special linear
   * group, which contains m.
   *
   * Canonical representives of a matrix in SL(2,C) have determinant
   * equal to one.
   */
  private def returnNormalizedToSL(mat: ComplexMatrix) = {
    val d = mat.det
    if (d == 0.0) throw NonInvertibleMatrixException(
      "Cannot normalize matrix with determinant zero."
    ) 
    val scalar = ComplexMatrix(1.0/d.sqrt, Complex(0.0+0.0), Complex(0.0+0.0), 1.0/d.sqrt)
    scalar * mat
  }

  /**
   * The transformation represented as an element of PSL(2,C).
   */
  val theTransformationMatrix = {
    try {
      returnNormalizedToSL(ComplexMatrix(a, b, c, d))
    } catch {
      case NonInvertibleMatrixException(_) => throw 
        NonInvertibleMatrixException("Attempting to create a non-invertible transformation.") 
    }
  }

  /**
   * Almost equality is defined up to equivalence class of PSL(2,C)
   *
   * To avoid problems due to rounding errors we consider two transformations
   * to be almost equal when they are 'close' with respect to the standard norm
   * on PSL(2,C). This is not a true equality relation as it is not transitive.
   */
  def almostEquals(that: MoebiusTransformation): Boolean = {
    val errorBounds = 0.0000000000001
    val mt1 = this.theTransformationMatrix + that.theTransformationMatrix
    val mt2 = this.theTransformationMatrix - that.theTransformationMatrix
    (matrixNorm(mt1) < errorBounds) ||  (matrixNorm(mt2) < errorBounds)
  }

  /**
   * The standard norm on PSL(2,C)
   */
  private def matrixNorm(m: ComplexMatrix): Double =
    (m.a.abs*m.a.abs + m.b.abs*m.b.abs + m.c.abs*m.c.abs + m.d.abs*m.d.abs).sqrt
  
}
object MoebiusTransformation {

  def apply(a: Complex[Double], b: Complex[Double], c: Complex[Double], d: Complex[Double]) = 
    new MoebiusTransformation(a: Complex[Double], b: Complex[Double], c: Complex[Double], d: Complex[Double])

  /**
   * The identity transformation.
   */
  val identity = MoebiusTransformation(1.0+0.0*i, 0.0 +0.0*i, 0.0+0.0*i, 1.0+0.0*i)

}
