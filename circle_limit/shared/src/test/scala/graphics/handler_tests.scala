package circle_limit.graphics

import scala.collection.immutable.Queue

import utest._
import diode.{ActionHandler,RootModelRW}
import diode.ActionResult.{ModelUpdate, NoChange}
import spire.implicits._

import circle_limit.maths.Imaginary.i
import circle_limit.graphics.{Converter, Box, Vector}


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
        case Some(ModelUpdate(x: Geometry)) => assert( x.handles contains Handle(initial.index, 0.5+0.1*i) )
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
        case Some(ModelUpdate(x: Geometry)) => assert( !(x.handles contains Handle(initial.index, 0.5+0.1*i)) )
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
        case Some(ModelUpdate(x: Geometry)) => assert( x.handles contains Handle(initial.index, 0.5+0.1*i) )
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
        handles = Set(Handle(23, 0.7+1.0*i), Handle(24, 0.5+1.0*i)),
        geodesics = Set((23,24)),
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
        case Some(ModelUpdate(x: Geometry)) => assert( x.geodesics contains (13, initial.index) )
        case _ => assert (false)
      }
    }

    "Move action moves the specified vertex" - {
      val initial = Geometry(
        handles = Set(Handle(23, 0.7+1.0*i), Handle(24, 0.5+1.0*i))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, MoveVertex(23, 0.6+0.2*i))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.handles contains Handle(23, 0.6+0.2*i) )
        case _ => assert (false)
      }
    }

    "Triple-click on vertex deletes it" - {
      val initial = Geometry(
        handles = Set(Handle(23, 0.7+1.0*i), Handle(24, 0.5+1.0*i))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, VertexTripleClick(23))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( !( g.handles contains Handle(23, 0.7+1.0*i) ))
        case _ => assert (false)
      }
    }

    "Triple-click on vertex deletes any geodesics attached to it" - {
      val initial = Geometry(
        handles = Set(Handle(23, 0.7+1.0*i), Handle(24, 0.5+1.0*i), Handle(25, -0.5+0.4*i)),
        geodesics = Set((23, 24), (24, 25), (25, 23))
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, VertexTripleClick(23))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.geodesics == Set((24, 25)) )
        case _ => assert (false)
      }
    }

    "Triple-click ensures all dangling vertices are deleted" - {
      val verticesToBeDeleted = Set(Handle(23, 0.7+1.0*i), Handle(24, 0.5+1.0*i))
      val verticesToBeKept = Set(Handle(25, -0.5+0.4*i), Handle(26, -0.2+0.2*i))
      val initial = Geometry(
        handles = verticesToBeDeleted union verticesToBeKept,
        geodesics = Set((23, 24), (25, 26))
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
        handles = Set(Handle(23, 0.7+1.0*i)),
        lastActive = Some(23)
      )
      val geometryHandler = new GeometryHandler(new RootModelRW(initial))

      val result = geometryHandler(initial, VertexTripleClick(23))

      result match {
        case Some(ModelUpdate(g: Geometry)) => assert ( g.lastActive.isEmpty )
        case _ => assert (false)
      }

    }

  }
}
