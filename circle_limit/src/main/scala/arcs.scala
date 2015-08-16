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

/**
 * Represents an arc of a circle.
 *
 * An instance of Arc with start=z_0, finish=z_1, and centre=z_2 should
 * be used to represent the segment of a circle with centre z_2, and 
 * radius |z_0 - z_2| drawn anti-clockwise from z_0 to z_1.
 */   
case class Arc(start: Complex, finish: Complex, centre: Complex)

/**
 * A Moebius transformation of form az+b/cz+d.
 */
class MoebiusTransformation(a: Complex, b: Complex, c: Complex, d: Complex) {

  /**
   * If mt is a MobiusTransformation and z a complex number, then 
   * mt transform z returns the image of z under mt.
   */
  def transform(z: Complex): Complex = (a*z + b) / (c*z + d)

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
