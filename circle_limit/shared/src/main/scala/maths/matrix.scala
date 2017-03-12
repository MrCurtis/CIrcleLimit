package circle_limit.maths

import spire.math.{
  Complex
}
import spire.algebra.Field
import spire.implicits._
import Imaginary.i


/**
 * A simple implementation of a two-by-two matrix. This is kept
 * purposefully simple and only has the features needed elsewhere in the
 * code.
 */
class Matrix[F: Field](val a: F, val b: F, val c: F, val d:F) {

  /**
   * Matrix multiplication.
   */
  def *(that: Matrix[F]) = 
    new Matrix(
      this.a * that.a + this.b * that.c,
      this.a * that.b + this.b * that.d,
      this.c * that.a + this.d * that.c,
      this.c * that.b + this.d * that.d
    )

  /*
   * Component-wise addition.
   */
  def +(that: Matrix[F]) = 
    new Matrix(
      this.a + that.a,
      this.b + that.b,
      this.c + that.c,
      this.d + that.d
    )

  /**
   * Component-wise subtraction.
   */
  def -(that: Matrix[F]) = 
    new Matrix(
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
   * Returns the inverse of the matrix
   */
  def inverse = {
    val k = 1/det
    new Matrix(
      k*d, -k*b,
      -k*c, k*a
    )

  }
  
  override def hashCode: Int = 
    41 * (
      41 * (
        41 * (
          41 + a.hashCode
        ) + b.hashCode
      ) + c.hashCode
    ) + d.hashCode

  def canEqual(other: Any) = other.isInstanceOf[Matrix[F]]

  /**
   * Transformation matrices are equal iff their elements are equal.
   *
   * Note that unlike Moebius transformations no normalizition occurs
   * before the matrices are compared.
   */
  override def equals (that: Any) = that match {
    case that: Matrix[F] => 
      (that canEqual this) &&
      this.a == that.a && this.b == that.b && this.c == that.c && this.d == that.d
    case _ => false
  }
}

object ComplexMatrix {

  type ComplexMatrix = Matrix[Complex[Double]]

  def apply(a: Complex[Double], b: Complex[Double], c: Complex[Double], d: Complex[Double]) = 
    new Matrix(a: Complex[Double], b: Complex[Double], c: Complex[Double], d: Complex[Double])

  /**
   * The identity matrix
   */
  val identity = new Matrix(1.0+0.0*i, 0.0 +0.0*i, 0.0+0.0*i, 1.0+0.0*i)
}
