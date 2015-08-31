package circle_limit

import breeze.linalg.{
  DenseMatrix,
  sum
}
import breeze.math.{
  Complex, 
  i
}
import breeze.numerics.{
  sqrt,
  abs
}


object CircleTypes {
  type ExtendedComplex = Either[Complex, Infinity.type]
  type ComplexPair = (Complex, Complex)
}
import CircleTypes._

object CircleImplicits {

  implicit def complexToEitherComplexOrInfinity(z: Complex): ExtendedComplex =
    Left(z)

  implicit def infinityToEitherComplexOrInfinity(z: Infinity.type): ExtendedComplex =
    Right(Infinity)

  implicit def complexToComplexPair(z: Complex): ComplexPair =
    (z, 1.0 + 0.0*i)

  implicit def infinityToComplexPair(z: Infinity.type): ComplexPair =
    (1.0 + 0.0*i, 0.0 + 0.0*i)

  implicit def complexToProjectiveComplex(z: Complex) =
    new ProjectiveComplex( complexToComplexPair(z) )

  implicit def infinityToProjectiveComplex(z: Infinity.type) =
    new ProjectiveComplex ( infinityToComplexPair(z) )

}

case class NonInvertibleMatrixException(err_msg: String) extends Exception(err_msg)

object Infinity
import Infinity._


/**
 * Represents a point on the projective projective line.
 *
 * This class is keeped simple for the time being. For example we 
 * do not define any operations.
 *
 */
class ProjectiveComplex(pair: ComplexPair){
  val z = pair._1
  val w = pair._2

  def equal(that: ProjectiveComplex) = {
    this.z * that.w == that.z * this.w
  }
}


/**
 * Represents an arc of a circle.
 *
 * An instance of Arc with start=z_0, finish=z_1, and centre=z_2 should
 * be used to represent the segment of a circle with centre z_2, and 
 * radius |z_0 - z_2| drawn anti-clockwise from z_0 to z_1.
 */   
case class Arc(start: Complex, finish: Complex, centre: Complex)


/**
 * Used to indicate which model of hyperbolic geometry is being used. 
 *
 * This is useful, for example, when we wish to define geodesics by
 * only specifying two points.
 */
object SpaceType extends Enumeration {
  val UpperHalfPlane = Value
  val PoincareDisc = Value
}


/**
 * A Moebius transformation of form az+b/cz+d.
 */
class MoebiusTransformation(a: Complex, b: Complex, c: Complex, d: Complex) {


  /**
   * If mt is a MobiusTransformation and z a point of the Reimann sphere, then 
   * mt transform z returns the image of z under mt.
   */
  def transform(projectiveComplex: ProjectiveComplex): ProjectiveComplex = 
    new ProjectiveComplex (
      a*projectiveComplex.z + b*projectiveComplex.w,
      c*projectiveComplex.z + d*projectiveComplex.w
    )

  /**
   * Returns the composite of the two transformations.
   */
  def compose(that: MoebiusTransformation) = {
    val composedMatrix = this.theTransformationMatrix * that.theTransformationMatrix
    new MoebiusTransformation(
      composedMatrix(0,0),
      composedMatrix(0,1),
      composedMatrix(1,0),
      composedMatrix(1,1)
    )
  }

  /**
   * The transformation represented as an element of PSL(2,C).
   */
  val theTransformationMatrix = normalize(DenseMatrix((a, b), (c, d)))

  /**
   * Returns a matrix in it's canonical form as an element of PSL(2,C).
   *
   * Canonical representives of a matrix in PSL(2,C) have determinant
   * equal to one.
   */
  private def normalize(unnormalized: DenseMatrix[Complex]): DenseMatrix[Complex] = {
    val d = det(unnormalized)
    if (d == 0.0) throw NonInvertibleMatrixException(
      "Attempting to create a non-invertible transformation."
    ) 
    unnormalized / srt(det(unnormalized))
  }

  /**
   * Returns the principal sqaureroot of a complex number.
   *
   * The principal squareroot is that which is contained in the upper 
   * half-plane union the real line.
   */
  private def srt(num: Complex): Complex = {
    if (num.imag == 0 && num.re <= 0) {
      0.0 + sqrt (-1.0 * num.re) * i
    }
    else {
      val r = abs(num)
      sqrt(r) * (num + r)/(abs(num + r))
    }
      
  }

  /**
   * Returns the determinant of a 2x2 matrix
   */
  private def det(matrix: DenseMatrix[Complex]): Complex = {
    matrix(0,0)*matrix(1,1) - matrix(0,1)*matrix(1,0)
  }

  /**
   * The standard norm on PSL(2,C)
   */
  private def matrixNorm(matrix: DenseMatrix[Complex]): Double = {
    sqrt(
      sum(
        matrix.map(z => z.abs*z.abs)
      )
    )
  }

  /**
   * Equality is defined up to equivalence class of PSL(2,C)
   *
   * To avoid problems due to rounding errors we consider two transformations
   * to be equal when they are 'close' with respect to the standard norm
   * on PSL(2,C)
   */
  def equal(that: MoebiusTransformation): Boolean = {
    val errorBounds = 0.0000000000001
    val mt1 = this.theTransformationMatrix + that.theTransformationMatrix
    val mt2 = this.theTransformationMatrix - that.theTransformationMatrix
    (matrixNorm(mt1) < errorBounds) ||  (matrixNorm(mt2) < errorBounds)
  }
}
