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
  type ExtendedComplex = Either[Infinity.type, Complex]
  type ComplexPair = (Complex, Complex)
}
import CircleTypes._

object CircleImplicits {

  implicit def complexToEitherComplexOrInfinity(z: Complex): ExtendedComplex =
    Right(z)

  implicit def infinityToEitherComplexOrInfinity(z: Infinity.type): ExtendedComplex =
    Left(Infinity)

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


