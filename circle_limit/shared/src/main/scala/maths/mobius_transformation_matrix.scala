package circle_limit.maths

import spire.math.{
  Complex
}
import spire.implicits._


/**
 * A two by two complex matrix used to represent a Moebius Transformation.
 *
 * The class is kept simple and only has the functionality needed for the
 * MoebiusTransformation class.
 */
class MoebiusTransformationMatrix(val a: Complex[Double], val b: Complex[Double], val c: Complex[Double], val d:Complex[Double]) {

  /**
   * Matrix multiplication.
   */
  def *(that: MoebiusTransformationMatrix) = 
    new MoebiusTransformationMatrix(
      this.a * that.a + this.b * that.c,
      this.a * that.b + this.b * that.d,
      this.c * that.a + this.d * that.c,
      this.c * that.b + this.d * that.d
    )

  /*
   * Component-wise addition.
   */
  def +(that: MoebiusTransformationMatrix) = 
    new MoebiusTransformationMatrix(
      this.a + that.a,
      this.b + that.b,
      this.c + that.c,
      this.d + that.d
    )

  /**
   * Component-wise subtraction.
   */
  def -(that: MoebiusTransformationMatrix) = 
    new MoebiusTransformationMatrix(
      this.a - that.a,
      this.b - that.b,
      this.c - that.c,
      this.d - that.d
    )

  /**
   * Returns the determinant of the matrix
   */
  def det = a*d - b*c

  /**
   * If m is a matrix in GL, the general linear group, then m.returnNormalizedToSL
   * returns the canonical element in the equivalence class of SL, the special linear
   * group, which contains m.
   *
   * Canonical representives of a matrix in SL(2,C) have determinant
   * equal to one.
   */
  def returnNormalizedToSL = {
    val d = this.det
    if (d == 0.0) throw NonInvertibleMatrixException(
      "Cannot normalize matrix with determinant zero."
    ) 
    val scalar = MoebiusTransformationMatrix(1.0/d.sqrt, Complex(0.0+0.0), Complex(0.0+0.0), 1.0/d.sqrt)
    scalar * this
  }
  
  override def hashCode: Int = 
    41 * (
      41 * (
        41 * (
          41 + a.hashCode
        ) + b.hashCode
      ) + c.hashCode
    ) + d.hashCode

  def canEqual(other: Any) = other.isInstanceOf[MoebiusTransformationMatrix]

  /**
   * Transformation matrices are equal iff their elements are equal.
   *
   * Note that unlike Moebius transformations no normalizition occurs
   * before the matrices are compared.
   */
  override def equals (that: Any) = that match {
    case that: MoebiusTransformationMatrix => 
      (that canEqual this) &&
      this.a == that.a && this.b == that.b && this.c == that.c && this.d == that.d
    case _ => false
  }
}

object MoebiusTransformationMatrix {
  def apply(a: Complex[Double], b: Complex[Double], c: Complex[Double], d: Complex[Double]) = 
    new MoebiusTransformationMatrix(a: Complex[Double], b: Complex[Double], c: Complex[Double], d: Complex[Double])
}
