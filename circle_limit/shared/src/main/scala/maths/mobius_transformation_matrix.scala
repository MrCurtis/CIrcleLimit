package circle_limit

import breeze.math.{
  Complex, 
  i
}
import breeze.numerics.{
  sqrt,
  abs
}


/**
 * A two by two complex matrix used to represent a Moebius Transformation.
 *
 * The class is kept simple and only has the functionality needed for the
 * MoebiusTransformation class.
 */
class MoebiusTransformationMatrix(val a: Complex, val b: Complex, val c: Complex, val d:Complex) {

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
    val scalar = MoebiusTransformationMatrix(1.0/srt(d), 0.0+0.0*i, 0.0+0.0*i, 1.0/srt(d))
    scalar * this
  }

  /**
   * Returns the principal square-root of a complex number.
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
  def apply(a: Complex, b: Complex, c: Complex, d: Complex) = 
    new MoebiusTransformationMatrix(a: Complex, b: Complex, c: Complex, d: Complex)
}
