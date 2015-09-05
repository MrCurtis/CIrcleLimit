package circle_limit

import breeze.math.i

import CircleTypes._

/**
 * Represents a point on the projective projective line.
 *
 * This class is kept simple for the time being. For example we 
 * do not define any operations.
 */
class ProjectiveComplex(pair: ComplexPair){
  val z = pair._1
  val w = pair._2

  override def hashCode: Int = 41 * ( 41 + z.hashCode ) + w.hashCode 

  def canEqual(other: Any) = other.isInstanceOf[ProjectiveComplex]

  override def equals (that: Any) = that match {
    case that: ProjectiveComplex => 
      (that canEqual this) &&
      (this.z * that.w == that.z * this.w)
    case _ => false
  }

  def toExtendedComplex: ExtendedComplex =
    if (w == 0.0 + 0.0*i) Left (Infinity)
    else Right(z/w)
}
