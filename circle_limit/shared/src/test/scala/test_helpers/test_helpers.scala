package circle_limit.test_helpers

import spire.implicits._

import circle_limit.maths.{Arc, Curve, Line, MoebiusTransformation}

/**
 * Helper functions for use in automated testing.
 */
object TestHelpers {

  /**
   * Checks that two sets of MoebiusTransformation are 'almost' equal.
   */
  def setsOfMoebiusTransformationAlmostEqual(
    set1: Set[MoebiusTransformation],
    set2: Set[MoebiusTransformation]
  ) = {
    set1.size == set2.size  &&
      set1.forall( x => set2.exists( y => x almostEquals y ) )
  }

  /**
   * Returns a boolean specifying whether two curves are 'almost' equal.
   */
  def curvesAlmostEqual(curve1: Curve, curve2: Curve): Boolean = {
    (curve1, curve2) match {
      case (arc1: Arc, arc2: Arc) => arcsAlmostEqual(arc1, arc2)
      case (line1: Line, line2: Line) => linesAlmostEqual(line1, line2)
      case (arc1: Arc, line2: Line) => arcAndLineAlmostEqual(arc1, line2)
      case (line1: Line, arc2: Arc) => arcAndLineAlmostEqual(arc2, line1)
    }
  }

  private val errorDelta = 0.005

  private def arcsAlmostEqual(arc1: Arc, arc2: Arc) = {
    (
      (arc1.start - arc2.start).abs < errorDelta && (arc1.finish - arc2.finish).abs < errorDelta
        && (arc1.centre - arc2.centre).abs < errorDelta
    )
  }

  private def linesAlmostEqual(line1: Line, line2: Line) = {
    (
      ((line1.start - line2.start).abs < errorDelta && (line1.finish - line2.finish).abs < errorDelta)
        || ((line1.start - line2.finish).abs < errorDelta && (line1.finish - line2.start).abs < errorDelta)
    )
  }

  private def arcAndLineAlmostEqual(arc: Arc, line: Line) = {
    (
      (((arc.start - line.start).abs < errorDelta && (arc.finish - line.finish).abs < errorDelta)
        || ((arc.start - line.finish).abs < errorDelta && (arc.finish - line.start).abs < errorDelta))
          && (arc.centre).abs > 1.0/errorDelta
    )

  }

}
