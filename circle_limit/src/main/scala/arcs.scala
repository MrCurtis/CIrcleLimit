package circle_limit

import spire.implicits._
import breeze.math.Complex
import breeze.math._

/**
 * Represents an arc of a circle.
 *
 * An instance of Arc with start=z_0, finish=z_1, and centre=z_2 should
 * be used to represent the segment of a circle with centre z_2, and 
 * radius |z_0 - z_2| drawn anti-clockwise from z_0 to z_1.
 */   
case class Arc(val start: Complex, val finish: Complex, val centre: Complex)
