package circle_limit

import breeze.math.Complex

abstract class Curve(val start: Complex, val finish: Complex)

/**
 * Represents a Euclidean line, with end-points start and finish.
 */
class Line(start: Complex, finish: Complex) extends Curve(start, finish)
object Line {
  def apply(start: Complex, finish: Complex) = new Line(start, finish)
}

/**
 * Represents an arc of a circle.
 *
 * An instance of Arc with start=z_0, finish=z_1, and centre=z_2 should
 * be used to represent the segment of a circle with centre z_2, and 
 * radius |z_0 - z_2| drawn anti-clockwise from z_0 to z_1.
 */   
class Arc(start: Complex, finish: Complex, val centre: Complex) extends Curve(start, finish)
object Arc {
  def apply(start: Complex, finish: Complex, centre: Complex) = new Arc(start, finish, centre)
}
