package circle_limit.maths

import spire.math.{
  Complex
}
import spire.implicits._
import Imaginary.i

/**
 * Represents all shapes that might be plotted.
 */
abstract class Shape

/**
 * Represents the geodesics.
 */
abstract class Curve extends Shape

/**
 * Represents a Euclidean line, with end-points start and finish.
 */
case class Line(start: Complex[Double], finish: Complex[Double]) extends Curve {

  def canEqual(other: Any) = other.isInstanceOf[Line]

  // TODO - not sure if this is a good hashcode, but any replacement 
  // must be symmetrical.
  override def hashCode: Int = start.hashCode + finish.hashCode

  override def equals (that: Any) = that match {
    case that: Line =>
      (that canEqual this) &&
      ( (that.start == this.start && that.finish == this.finish) || 
        (that.start == this.finish && that.finish == this.start))
    case _ => false
  }
}

/**
 * Represents an arc of a circle.
 *
 * An instance of Arc with start=z_0, finish=z_1, and centre=z_2 should
 * be used to represent the segment of a circle with centre z_2, and 
 * radius |z_0 - z_2| drawn anti-clockwise from z_0 to z_1.
 */   
case class Arc(start: Complex[Double], finish: Complex[Double], centre: Complex[Double]) extends Curve {

  def canEqual(other: Any) = other.isInstanceOf[Arc]

  override def hashCode: Int = 
    41 * (
      41 * (
        41 + start.hashCode
      ) + finish.hashCode
    ) + centre.hashCode

  override def equals(other: Any) = other match {
    case that: Arc => 
      (that canEqual this) &&
      that.start == this.start &&
      that.finish == this.finish &&
      that.centre == this.centre
    case _ => false
  }

  /**
   * Returns true only if the arc subtends an angle of less than Pi.
   */
  def isAcute = ((finish - centre) / (start - centre)).imag > 0

  /**
   * Returns true only if the arc subtends an angle of more than Pi.
   */
  def isObtuse = ((finish - centre) / (start - centre)).imag < 0

  /**
   * Returns the complement of the arc with respect to its associated circle.
   */
  def complement = Arc(finish, start, centre)

  /**
   * Returns the acute arc with the same end points and centre.
   *
   * If the arc is neither obtuse or acute then then method returns 
   * the object it is called from.
   */
  def getAcute = if (this.isObtuse) this.complement else this

  /**
   * Returns the obtuse arc with the same end points and centre.
   *
   * If the arc is neither obtuse or acute then then method returns 
   * the object it is called from.
   */
  def getObtuse = if (this.isAcute) this.complement else this
    
}

/**
 * Represents a Euclidean circle.
 */
case class Circle(centre: Complex[Double], radius: Double) extends Shape {

  def canEqual(other: Any) = other.isInstanceOf[Circle]

  override def hashCode: Int = 
    41 * (
      41 + centre.hashCode
    ) + radius.hashCode

  override def equals(other: Any) = other match {
    case that: Circle => 
      (that canEqual this) &&
      that.centre == this.centre &&
      that.radius == this.radius
    case _ => false
  }
}
