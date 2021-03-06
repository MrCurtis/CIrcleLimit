package circle_limit.graphics

import scala.collection.immutable.SortedMap
import scala.math.sqrt

import utest._
import diode.{ActionHandler,RootModelRW}
import diode.ActionResult.{ModelUpdate, NoChange}
import spire.implicits._

import circle_limit.maths.Imaginary.i
import circle_limit.maths.Group


object ConverterHandlerTestSuite extends TestSuite {
  val tests = TestSuite {
    "Test sets converter correctly" - {
      val initial = Converter(
        Box(-1.05, -1.05, 2.10, 2.10),
        Box(0.0, 0.0, 100, 200)
      )
      val converterHandler = new ConverterHandler(new RootModelRW(initial))

      val result = converterHandler(initial, Resize(50, 300))

      val graphicalPoints = List(Vector(0,175), Vector(50, 175), Vector(0, 125))
      val expectedMathematicalPoints = List(-1.05 + -1.05*i, 1.05 -1.05*i, -1.05 + 1.05*i)

      result match {
        case Some(ModelUpdate(converter)) => {
          assert(
            graphicalPoints.map(converter.convertFromGraphicalToMathematicalSpace(_))
              == expectedMathematicalPoints
          )
        }
        case _ => assert (false)
      }
    }
  }
}


object GeometryHandlerTestSuite extends TestSuite {
  val tests = TestSuite {

    "Double-click creates vertex if not active" - {
      val initial = Geometry(
        index = 12,
        lastActive = None
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasDoubleClick(0.5+0.1*i))

      result match {
        case Some(ModelUpdate(x: Geometry)) => assert( x.handles.toSet contains (initial.index, 0.5+0.1*i) )
        case _ => assert (false)
      }
    }

    "Double-click has no effect if outside circle" - {
      val initial = Geometry(
        index = 25,
        handles = SortedMap((23, 0.7+1.0*i), (24, 0.5+1.0*i)),
        geodesics = List((23,24)),
        lastActive = Some(24)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasDoubleClick(2.5+0.1*i))

      result match {
        case Some(NoChange) =>
        case _ => assert (false)
      }
    }

    "Double-click does not create vertex if active - (it's created by the first single click)" - {
      val initial = Geometry(
        index = 12,
        lastActive = Some(11)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasDoubleClick(0.5+0.1*i))

      result match {
        case None =>
        case Some(ModelUpdate(x: Geometry)) => assert( !(x.handles.toSet contains (initial.index, 0.5+0.1*i)) )
        case _ => assert (false)
      }
    }

    "Creating a vertex through a double click increases index" - {
      val initial = Geometry(
        index = 12,
        lastActive = None
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasDoubleClick(0.5+0.1*i))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert( g.index == initial.index + 1 )
        case _ => assert (false)
      }
    }

    "Single-click creates vertex if active" - {
      val initial = Geometry(
        index = 12,
        lastActive = Some(8)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasSingleClick(0.5+0.1*i))

      result match {
        case Some(ModelUpdate(x: Geometry)) => assert( x.handles.toSet contains (initial.index, 0.5+0.1*i) )
        case _ => assert (false)
      }
    }

    "Creating a vertex through a single click increases index" - {
      val initial = Geometry(
        index = 12,
        lastActive = Some(8)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasSingleClick(0.5+0.1*i))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert( g.index == initial.index + 1 )
        case _ => assert (false)
      }
    }

    "Single-click does not create vertex if not active" - {
      val initial = Geometry(
        index = 12,
        lastActive = None
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasSingleClick(0.5+0.1*i))

      result match {
        case Some(NoChange) =>
        case _ => assert (false)
      }
    }

    "Double-click on active canvas makes it in-active" - {
      val initial = Geometry(
        lastActive = Some(100)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasDoubleClick(0.5+0.1*i))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert( g.lastActive.isEmpty )
        case _ => assert (false)
      }
    }

    "Double-click on active vertex makes inactive" - {
      val initial = Geometry(
        index = 25,
        handles = SortedMap((23, 0.7+1.0*i), (24, 0.5+1.0*i)),
        geodesics = List((23,24)),
        lastActive = Some(24)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, VertexDoubleClick(24))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert( g.lastActive.isEmpty )
        case _ => assert (false)
      }
    }

    "Single-click on canvas creates geodesic if active" - {
      val initial = Geometry(
        lastActive = Some(13)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasSingleClick(0.5+0.1*i))

      result match {
        case Some(ModelUpdate(x: Geometry)) => assert( x.geodesics.toSet contains (13, initial.index) )
        case _ => assert (false)
      }
    }

    "Single-click outside of circle does not create vertex" - {
      val initial = Geometry(
        lastActive = Some(13)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, CanvasSingleClick(1.5+0.1*i))

      result match {
        case Some(NoChange) =>
        case _ => assert (false)
      }
    }

    "Move action moves the specified vertex" - {
      val initial = Geometry(
        handles = SortedMap((23, 0.7+1.0*i), (24, 0.5+1.0*i))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, MoveVertex(23, 0.6+0.2*i))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.handles.toSet contains (23, 0.6+0.2*i) )
        case _ => assert (false)
      }
    }

    "Move action only moves vertex to boundary" - {
      val initial = Geometry(
        handles = SortedMap((23, 0.7+1.0*i), (24, 0.5+1.0*i))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, MoveVertex(23, 1+1*i))
      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.handles.toSet contains (23, 1.0/2.0.sqrt+1.0/2.0.sqrt*i))
        case _ => assert (false)
      }
    }

    "Move action can move to the origin" - {
      val initial = Geometry(
        handles = SortedMap((23, 0.7+1.0*i), (24, 0.5+1.0*i))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, MoveVertex(23, 0+0*i))
      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.handles.toSet contains (23, 0+0*i))
        case _ => assert (false)
      }
    }

    "Triple-click on vertex deletes it" - {
      val initial = Geometry(
        handles = SortedMap((23, 0.7+1.0*i), (24, 0.5+1.0*i))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, VertexTripleClick(23))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( !( g.handles.toSet contains (23, 0.7+1.0*i) ))
        case _ => assert (false)
      }
    }

    "Triple-click on vertex deletes any geodesics attached to it" - {
      val initial = Geometry(
        handles = SortedMap((23, 0.7+1.0*i), (24, 0.5+1.0*i), (25, -0.5+0.4*i)),
        geodesics = List((23, 24), (24, 25), (25, 23))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, VertexTripleClick(23))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.geodesics == List((24, 25)) )
        case _ => assert (false)
      }
    }

    "Triple-click ensures all dangling vertices are deleted" - {
      val verticesToBeDeleted = SortedMap((23, 0.7+1.0*i), (24, 0.5+1.0*i))
      val verticesToBeKept = SortedMap((25, -0.5+0.4*i), (26, -0.2+0.2*i))
      val initial = Geometry(
        handles = verticesToBeDeleted ++ verticesToBeKept,
        geodesics = List((23, 24), (25, 26))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, VertexTripleClick(23))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.handles == verticesToBeKept )
        case _ => assert (false)
      }
    }

    "Triple click on vertex sets to not active - this is to avoid strange behaviour with double double-click" - {
      val initial = Geometry(
        handles = SortedMap((23, 0.7+1.0*i)),
        lastActive = Some(23)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, VertexTripleClick(23))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.lastActive.isEmpty )
        case _ => assert (false)
      }
    }

    "Can mark vertex as active" - {
      val initial = Geometry(activeVertex = None)
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, MakeVertexActive(23))

      result match {
        case Some(ModelUpdate(Geometry( _, _, _, _, Some(23)))) => assert (true)
        case _ => assert (false)
      }

    }

    "Can make all vertices inactive" - {
      val initial = Geometry(activeVertex = Some(23))
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, MakeVerticesInactive)

      result match {
        case Some(ModelUpdate(Geometry( _, _, _, _, None))) => assert (true)
        case _ => assert (false)
      }
    }

  }
}


object GroupHandlerTestSuite extends TestSuite {
  val tests = TestSuite {

    "Sets group to selected" - {
      val initial = Group.torsionFreeGroup(2)
      val newGroup  = Group.torsionFreeGroup(2)
      val groupHandler = new GroupHandler(new RootModelRW(initial))

      val result = groupHandler(initial, SelectGroup(newGroup))

      result match {
        case Some(ModelUpdate(`newGroup`)) =>
        case _ => assert (false)
      }
    }
  }
}


object FadingHandlerTestSuite extends TestSuite {
  val tests = TestSuite {

    "Hide-controller event sets to hidden if currently showing" - {
      val initial = Show
      val fadingHandler = new FadingHandler(new RootModelRW(initial))

      val result = fadingHandler(initial, HideControls)

      result match {
        case Some(ModelUpdate(Hide)) =>
        case _ => assert(false)
      }
    }

    "Hide-controller event has no effect if currently hidden" - {
      val initial = Hide
      val fadingHandler = new FadingHandler(new RootModelRW(initial))

      val result = fadingHandler(initial, HideControls)

      result match {
        case Some(NoChange) =>
        case _ => assert (false)
      }
    }

    "Show-controller event sets to show if currently hidden" - {
      val initial = Hide
      val fadingHandler = new FadingHandler(new RootModelRW(initial))

      val result = fadingHandler(initial, ShowControls)

      result match {
        case Some(ModelUpdate(Show)) =>
        case _ => assert(false)
      }
    }

    "Show-controller event has no effect if currently showing" - {
      val initial = Show
      val fadingHandler = new FadingHandler(new RootModelRW(initial))

      val result = fadingHandler(initial, ShowControls)

      result match {
        case Some(NoChange) =>
        case _ => assert (false)
      }
    }
  }
}
