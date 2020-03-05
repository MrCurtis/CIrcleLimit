package circle_limit.test_helpers

import utest._

import spire.implicits._

import circle_limit.maths.Imaginary.i
import circle_limit.maths.CircleImplicits._

import circle_limit.maths.{Arc, Curve, Line, MoebiusTransformation}
import TestHelpers.{setsOfMoebiusTransformationAlmostEqual, curvesAlmostEqual}


object CurvesAlmostEqualTestSuite extends TestSuite {

  val testSet = Set(
    Arc(1,2,3),
    Line(1,2),
    Line(2,1),
    Arc(0.456, 0.123, 1.2),
    Arc(0.466, 0.123, 1.2),
    Arc(0.456, 0.113, 1.2),
    Arc(0.456, 0.133, 1.2),
    Arc(0.456, 0.123, 1.21),
    Arc(0.466, 0.123, 1.2),
    Arc(0.113, 0.456, 1.2),
    Arc(0.112, 0.452, 1.2),
    Arc(0.112, 0.452, 23.4),
    Arc(0.456, 0.11323, 1.23),
    Line(0.456, 0.123),
    Line(0.454, 0.123),
    Line(0.454, 0.120),
    Line(0.454, 0.120),
    Line(0.123, 0.454),
    Line(0.124, 0.454),
    Line(0.123, 0.455),
    Line(0.230, 0.450),
    Line(-3019.0001, 234.2),
    Line(0.124, -0.454),
    Arc(12.4, 45.7, 199.0),
    Arc(12.4, 45.7, 204.0),
    Arc(12.4, 45.7, 204.0),
    Line(12.4, 45.7),
    Line(45.7, 12.401),
    Arc(0.456+0.2*i, 0.123, 1.2+0.123*i),
    Arc(0.466, 0.123, 1.2),
    Arc(145.67*i, 0.456, 0.001),
    Arc(0.112, 0.452, 1.2),
    Arc(12+0.000*i, 0.452, 23.4),
    Arc(0.456, 0.11323, 1.23),
    Line(0.456, 0.123),
    Line(0.454, 0.123),
    Line(0.454, 0.120),
    Line(0.454, 0.120),
    Line(0.123, 0.454),
    Line(0.124, 0.454),
    Arc(12.4, 45.7, 199.0+19.9*i),
    Arc(12.4, 45.7, 199.0+20*i),
    Line(12.4+0.0001*i, 45.7)
  )

  val testProducts = for{x <- testSet; y <- testSet} yield (x, y)

  val tests = TestSuite {

    val almostEqual = testProducts.filter(x => curvesAlmostEqual(x._1, x._2))
    val notAlmostEqual = testProducts -- almostEqual

    "Curves should be almost equal if they are equal" - {
      val equal = testProducts.filter(x => x._1 == x._2)
      assert (equal subsetOf almostEqual)
    }
    "curvesAlmostEqual should be symetric" - {
      assert (almostEqual == almostEqual.map(x => (x._2, x._1)))
    }
    "Using notation x~=y to mean abs(x-y) < 0.005:" -  {
      "Arc(s1, f1, c1) and Arc(s1, f1, c1) should not be almost equal if !(s1~=s2)" - {
        val arcsWithStartNotAlmostEqual =
          testProducts.filter{
            case (Arc(s1,_,_), Arc(s2,_,_)) => (s2-s1).abs >= 0.005
            case _ => false
          }
        assert (arcsWithStartNotAlmostEqual subsetOf notAlmostEqual)
      }
      "Arc(s1, f1, c1) and Arc(s1, f1, c1) should not be almost equal if !(f1~=f2)" - {
        val arcsWithFinishNotAlmostEqual =
          testProducts.filter{
            case (Arc(_,f1,_), Arc(_,f2,_)) => (f2-f1).abs >= 0.005
            case _ => false
          }
        assert (arcsWithFinishNotAlmostEqual subsetOf notAlmostEqual)
      }
      "Arc(s1, f1, c1) and Arc(s1, f1, c1) should not be almost equal if !(c1~=c2)" - {
        val arcsWithCentreNotAlmostEqual =
          testProducts.filter{
            case (Arc(_,_,c1), Arc(_,_,c2)) => (c2-c1).abs >= 0.005
            case _ => false
          }
        assert (arcsWithCentreNotAlmostEqual subsetOf notAlmostEqual)
      }
      "Line(s1, f1) and Line(s2, f2) should be almost equal if s1~=s2 and f1~=f2" - {
        val linesWithStartsAlmostEqualAndFinishesAlmostEqual =
          testProducts.filter{
            case (Line(s1,f1), Line(s2,f2)) => (s2-s1).abs < 0.005 && (f2-f1).abs < 0.005
            case _ => false
          }
        assert(linesWithStartsAlmostEqualAndFinishesAlmostEqual subsetOf almostEqual)
      }
      "Line(s1, f1) and Line(s2, f2) should be almost equal if s1~=f2 and f1~=s2" - {
        val linesWithStartsAndFinishesAlmostEqual =
          testProducts.filter {
            case (Line(s1,f1), Line(s2,f2)) => (s2-f1).abs < 0.005 && (f2-s1).abs < 0.005
            case _ => false
          }
        assert(linesWithStartsAndFinishesAlmostEqual subsetOf almostEqual)
      }
      "Arc(s1, f1, c1) and Line(s2, f2) should not be almost equal if abs(c1) <= 200" - {
        val arcsWithCentreDistanceSmall =
          testProducts.filter {
            case (Arc(_, _,c), Line(_,_)) => c.abs <= 200
            case _ => false
          }
        assert(arcsWithCentreDistanceSmall subsetOf notAlmostEqual)
      }
      "Arc(s1, f1, c1) and Line(s2, f2) should be almost equal if s1~=s2, f1~=f2 and c1 > 200" - {
        val pointsAlmostEqualAndCentreDistanceLarge =
          testProducts.filter {
            case (Arc(s1,f1,c1), Line(s2,f2)) => {
              (s2-s1).abs < 0.005 && (f2-f1).abs < 0.005 && c1.abs > 200
            }
            case _ => false
          }
        assert(pointsAlmostEqualAndCentreDistanceLarge subsetOf almostEqual)
      }
      "Arc(s1, f1, c1) and Line(s2, f2) should be almost equal if s1~=f2, f1~=s2 and c1 > 200" - {
        val pointsReversedAlmostEqualAndCentreDistanceLarge =
          testProducts.filter {
            case (Arc(s1,f1,c1), Line(s2,f2)) => {
              (s2-f1).abs < 0.005 && (f2-s1).abs < 0.005 && c1.abs > 200
            }
            case _ => false
          }
        assert(pointsReversedAlmostEqualAndCentreDistanceLarge subsetOf almostEqual)
      }
    }
  }
}

object CheckSetsOfMoebiusTransformationsAlmostEqual extends TestSuite {

  val tests = TestSuite {

    "Empty set is almost equal to the empty set." - {
      val set1 = Set[MoebiusTransformation]()
      val set2 = Set[MoebiusTransformation]()
      assert (
        setsOfMoebiusTransformationAlmostEqual(set1, set2)
      )
    }

    "Sets almost equal if elements are almost equal" - {
      val set1 = Set(
        MoebiusTransformation(
          -0.6068072407032272 + 6.04794368359524727*i,
          0.0055050801181003 + 7.3975392397193067*i,
          -8.5410362205371368 + 8.22379800169699995*i,
          91.895627962623881 + 1.9187422792593443*i
        ),
        MoebiusTransformation(
          0.2062072401072255 + 4.04494168029824927*i,
          1.4565750809181804 - 0.4775894393197067*i,
          1.5310362005361362 - 0.22470234275677775*i,
          7.845567575673881 + 1.9187422756793443*i
        ),
        MoebiusTransformation(
          2.2533372406572255 + 3.04567165629856925*i,
          -45.4565750809185504 - 0.4774794333147556*i,
          1.5310362005361352 + 54.24560456254677775*i,
          7.845566873577751 - 1.9145456756793563*i
        )
      )
      val set2 = Set(
        MoebiusTransformation(
          -0.6068072407032277 + 6.04794368359524727*i,
          0.0055050801181007 + 7.3975392397193037*i,
          -8.5410362205371368 + 8.22379800169699935*i,
          91.895627962623861 + 1.9187422792593444*i
        ),
        MoebiusTransformation(
          0.2062072401072252 + 4.04494168029824920*i,
          1.4565750809181804 - 0.4775894393197067*i,
          1.5310362005361367 - 0.22470234275677777*i,
          7.845567575673880 + 1.9187422756793443*i
        ),
        MoebiusTransformation(
          2.2533372406572225 + 3.04567165629856925*i,
          -45.4565750809185502 - 0.4774794333147556*i,
          1.5310362005361352 + 54.24560456254677779*i,
          7.845566873577753 - 1.9145456756793563*i
        )
      ) // only the last two digits of some entries vary

      assert (
        setsOfMoebiusTransformationAlmostEqual(set1, set2)
      )

    }

    "Sets not almost equal if one element of each set differs" - {
      val set1 = Set(
        MoebiusTransformation(
          -0.3062072401072272 + 0.04494168029824927*i,
          1.0085050809181804 + 0.4775894393197067*i,
          -1.5310362005361362 + 0.22470840149124635*i,
          1.845627962623881 + 1.9187422792593443*i
        ),
        MoebiusTransformation(
          0.2062072401072255 + 4.04494168029824927*i,
          1.4565750809181804 - 0.4775894393197067*i,
          1.5310362005361362 - 0.22470234275677775*i, // This is the entry that differs from below
          7.845567575673881 + 1.9187422756793443*i
        ),
        MoebiusTransformation(
          2.2533372406572255 + 3.04567165629856925*i,
          -45.4565750809185504 - 0.4774794333147556*i,
          1.5310362005361352 + 54.24560456254677775*i,
          7.845566873577751 - 1.9145456756793563*i
        )
      )
      val set2 = Set(
        MoebiusTransformation(
          -0.3062072401072272 + 0.04494168029824927*i,
          1.0085050809181804 + 0.4775894393197067*i,
          -1.5310362005361362 + 0.22470840149124635*i,
          1.845627962623881 + 1.9187422792593443*i
        ),
        MoebiusTransformation(
          0.2062072401072255 + 4.04494168029824927*i,
          1.4565750809181804 - 0.4775894393197067*i,
          1.7310362005361362 - 0.22470234275677775*i, // This is the entry that differs from above
          7.845567575673881 + 1.9187422756793443*i
        ),
        MoebiusTransformation(
          2.2533372406572255 + 3.04567165629856925*i,
          -45.4565750809185504 - 0.4774794333147556*i,
          1.5310362005361352 + 54.24560456254677775*i,
          7.845566873577751 - 1.9145456756793563*i
        )
      )

      assert (
        ! setsOfMoebiusTransformationAlmostEqual(set1, set2)
      )

    }

    "Sets not almost equal if first is almost equal to a proper subset of the second" - {
      val set1 = Set(
        MoebiusTransformation(
          -0.6068072407032272 + 6.04794368359524727*i,
          0.0055050801181003 + 7.3975392397193067*i,
          -8.5410362205371368 + 8.22379800169699995*i,
          91.895627962623881 + 1.9187422792593443*i
        ),
        MoebiusTransformation(
          2.2533372406572255 + 3.04567165629856925*i,
          -45.4565750809185504 - 0.4774794333147556*i,
          1.5310362005361352 + 54.24560456254677775*i,
          7.845566873577751 - 1.9145456756793563*i
        )
      )
      val set2 = Set(
        MoebiusTransformation(
          -0.6068072407032277 + 6.04794368359524727*i,
          0.0055050801181007 + 7.3975392397193037*i,
          -8.5410362205371368 + 8.22379800169699935*i,
          91.895627962623861 + 1.9187422792593444*i
        ),
        MoebiusTransformation(
          0.2062072401072252 + 4.04494168029824920*i,
          1.4565750809181804 - 0.4775894393197067*i,
          1.5310362005361367 - 0.22470234275677777*i,
          7.845567575673880 + 1.9187422756793443*i
        ),
        MoebiusTransformation(
          2.2533372406572225 + 3.04567165629856925*i,
          -45.4565750809185502 - 0.4774794333147556*i,
          1.5310362005361352 + 54.24560456254677779*i,
          7.845566873577753 - 1.9145456756793563*i
        )
      )

      assert (
        ! setsOfMoebiusTransformationAlmostEqual(set1, set2)
      )

    }

    "Sets not almost equal if second is almost equal to a proper subset of the first" - {
      val set1 = Set(
        MoebiusTransformation(
          -0.6068072407032272 + 6.04794368359524727*i,
          0.0055050801181003 + 7.3975392397193067*i,
          -8.5410362205371368 + 8.22379800169699995*i,
          91.895627962623881 + 1.9187422792593443*i
        ),
        MoebiusTransformation(
          0.2062072401072252 + 4.04494168029824920*i,
          1.4565750809181804 - 0.4775894393197067*i,
          1.5310362005361367 - 0.22470234275677777*i,
          7.845567575673880 + 1.9187422756793443*i
        ),
        MoebiusTransformation(
          2.2533372406572255 + 3.04567165629856925*i,
          -45.4565750809185504 - 0.4774794333147556*i,
          1.5310362005361352 + 54.24560456254677775*i,
          7.845566873577751 - 1.9145456756793563*i
        )
      )
      val set2 = Set(
        MoebiusTransformation(
          -0.6068072407032277 + 6.04794368359524727*i,
          0.0055050801181007 + 7.3975392397193037*i,
          -8.5410362205371368 + 8.22379800169699935*i,
          91.895627962623861 + 1.9187422792593444*i
        ),
        MoebiusTransformation(
          2.2533372406572225 + 3.04567165629856925*i,
          -45.4565750809185502 - 0.4774794333147556*i,
          1.5310362005361352 + 54.24560456254677779*i,
          7.845566873577753 - 1.9145456756793563*i
        )
      )

      assert (
        ! setsOfMoebiusTransformationAlmostEqual(set1, set2)
      )

    }
  }
}
