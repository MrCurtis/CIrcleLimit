package circle_limit.graphics

import scala.collection.immutable.SortedMap

import spire.math.Complex
import spire.implicits._
import diode.{Action, ActionHandler, ModelRW}

import circle_limit.maths.Group

case class Geometry(
  index: Int=1,
  handles: SortedMap[Int, Complex[Double]]=SortedMap(),
  geodesics: List[(Int,Int)]=List(),
  lastActive: Option[Int]=None
)
case class Root(
  converter: Converter,
  geometry: Geometry=Geometry(),
  group: Group=Group.trivialGroup,
  visibility: Visibility=Show
)
sealed trait Visibility
case object Show extends Visibility
case object Hide extends Visibility


case class MoveVertex(index: Int, position: Complex[Double]) extends Action
case class Resize(width: Double, height: Double) extends Action
case class CanvasDoubleClick(position:  Complex[Double]) extends Action
case class CanvasSingleClick(position:  Complex[Double]) extends Action
case class VertexDoubleClick(id: Int) extends Action
case class VertexTripleClick(id: Int) extends Action
case class SelectGroup(group: Group) extends Action
case object HideControls extends Action
case object ShowControls extends Action


class ConverterHandler[M](modelRW: ModelRW[M, Converter]) extends ActionHandler(modelRW) {
  override def handle = {
    case Resize(width, height) => {
      updated(
        Converter(
          Box(-1.05, -1.05, 2.10, 2.10),
          Box(0.0, 0.0, width, height)
        )
      )
    }
  }
}


class GeometryHandler[M](modelRW: ModelRW[M, Geometry]) extends ActionHandler(modelRW) {

  override def handle = {
    case CanvasSingleClick(position) => handleCanvasSingleClick(position)
    case CanvasDoubleClick(position) => handleCanvasDoubleClick(position)
    case MoveVertex(id, position) => handleMoveVertex(id, position)
    case VertexDoubleClick(id) => handleVertexDoubleClick(id)
    case VertexTripleClick(id) => handleVertexTripleClick(id)
  }

  private def handleCanvasSingleClick(position: Complex[Double]) = {
    if (position.norm > 1) {
      noChange
    } else {
      val geometry = modelRW.value
      geometry.lastActive match {
        case Some(lastIndex) => updated(
          geometry.copy(
            index = geometry.index + 1,
            handles = geometry.handles + (geometry.index -> position),
            geodesics = geometry.geodesics :+ (lastIndex, geometry.index),
            lastActive = Some(geometry.index)
          )
        )
        case None => noChange
      }
    }
  }

  private def handleCanvasDoubleClick(position: Complex[Double]) = {
    if (position.norm > 1) {
      noChange
    } else {
      val geometry = modelRW.value
      geometry.lastActive match {
        case Some(_) => {
          updated(
            geometry.copy(
              lastActive = None
            )
          )
        }
        case None => {
          updated(
            geometry.copy(
              index = geometry.index + 1,
              handles = geometry.handles + (geometry.index -> position),
              lastActive = Some(geometry.index)
            )
          )
        }
      }
    }
  }

  private def handleMoveVertex(id: Int, position: Complex[Double]) = {
    val geometry = modelRW.value
    updated(
      geometry.copy(
        handles = geometry.handles + (id -> position/(1 max position.norm))
      )
    )
  }

  private def handleVertexDoubleClick(id: Int) = {
    updated(modelRW.value.copy(lastActive=None))
  }

  private def handleVertexTripleClick(id: Int) = {
    val geometry = modelRW.value
    val keptGeodesics = geometry.geodesics.filter(g => ! (g._1 == id || g._2 == id))
    val keptVerticesIds = keptGeodesics.foldLeft(Set(): Set[Int])((s, g) => s union Set(g._1, g._2))
    updated(
      geometry.copy(
        handles = geometry.handles.filterKeys(keptVerticesIds contains _),
        geodesics = keptGeodesics,
        lastActive = None
      )
    )
  }

}


class GroupHandler[M](modelRW: ModelRW[M, Group]) extends ActionHandler(modelRW) {
  override def handle = {
    case SelectGroup(g) => updated(g)
  }
}


class FadingHandler[M](modelRW: ModelRW[M, Visibility]) extends ActionHandler(modelRW) {
  override def handle = {
    case HideControls => {
      modelRW.value match {
        case Show => updated(Hide)
        case Hide => noChange
      }
    }
    case ShowControls => {
      modelRW.value match {
        case Hide => updated(Show)
        case Show => noChange
      }
    }
  }
}
