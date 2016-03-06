package circle_limit.maths

/**
 * Helper functions for use in automated testing.
 */
object TestHelpers {

  /**
   * Checks that two sets of MoebiusTransformation are 'almost' equal.
   */
  def checkSetsOfMoebiusTransformationAlmostEqual(
    set1: Set[MoebiusTransformation],
    set2: Set[MoebiusTransformation]
  ) = {
    set1.size == set2.size  &&
      set1.forall( x => set2.exists( y => x almostEquals y ) )
  }

}
