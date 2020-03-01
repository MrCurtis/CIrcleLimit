package circle_limit

import scala.collection.immutable.SortedMap

import scala.scalajs.js

import org.scalajs.dom.window
import org.scalajs.dom.document
import org.scalajs.dom.raw.MouseEvent
import spire.math.Complex
import spire.implicits._
import diode.NoAction
import diode.react.ModelProxy

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.svg_<^._
import japgolly.scalajs.react.vdom.html_<^.<.button
import japgolly.scalajs.react.vdom.html_<^.^.{key, onClick, onMouseUp, onMouseDown, onMouseMove}

import circle_limit.maths.CircleImplicits._
import circle_limit.maths.Imaginary.i
import circle_limit.maths.{Geodesic, SpaceType, Group, MoebiusTransformation}
import circle_limit.graphics.{Converter, Vector}
import circle_limit.graphics.AppCircuit
import circle_limit.graphics.{
  Root,
  CanvasSingleClick,
  CanvasDoubleClick,
  MoveVertex,
  SelectGroup,
  VertexDoubleClick,
  VertexTripleClick,
  Visibility,
  Resize,
  HideControls,
  ShowControls,
  Hide,
  Show
}


object Canvas {

  case class Props(modelProxy: ModelProxy[Root])

  case class State(vertexSelected: Option[Int])

  class Backend(bs: BackendScope[Props,State]) {
    def render(props: Props, state: State) = {

      val model = props.modelProxy()

      def handleClick(event: ReactMouseEventFromInput) = {
        props.modelProxy.dispatchCB({
          val converter = props.modelProxy().converter
          val posMathematical = converter.convertFromGraphicalToMathematicalSpace(
            Vector(event.clientX.toString.toDouble, event.clientY.toString.toDouble))
          if (event.detail == 2) {
            CanvasDoubleClick(posMathematical)
          } else {
            CanvasSingleClick(posMathematical)
          }
        })
      }

      def handleMouseDown(elementID: Int) = bs.setState(State(Some(elementID)))
      def handleMouseUp() = bs.setState(State(None))

      val vertices = model.geometry.handles
      val toGraphical = model.converter.convertFromMathematicalToGraphicalSpace(_)
      def vertexElement(visibility: Visibility)(vertex: (Int, Complex[Double])) = {
          vertex match {
            case (id, position) => VertexHandle(
              props.modelProxy,
              toGraphical(position),
              visibility,
              handleMouseDown,
              handleMouseUp,
              handleMouseMove(state.vertexSelected, props.modelProxy)(_),
              id
            )
          }
      }
      val geodesicElements = for {
        handlePair <- handlePairsFromGeodesics(model.geometry.handles, model.geometry.geodesics)
        groupElement <- model.group.elements
      } yield GeodesicView(handlePair._1, handlePair._2, groupElement, model.converter)

      <.svg(
        <.rect(
          ^.id := "canvas",
          ^.height := "100%",
          ^.width := "100%",
          ^.fill := "white",
          onClick ==> handleClick,
          onMouseMove ==> handleMouseMove(state.vertexSelected, props.modelProxy),
          onMouseUp --> handleMouseUp
        ),
        BoundaryCircle(model.converter),
        geodesicElements.toTagMod,
        vertices.toTagMod(vertexElement(model.visibility) _),
        GroupSelector(props.modelProxy, model.group, model.converter, model.visibility),
      )
    }

    def handleMouseMove(vertexSelected: Option[Int], modelProxy: ModelProxy[Root])(e: ReactMouseEventFromInput) = {
        vertexSelected match {
          case Some(id) => {
            val newPosition = modelProxy().converter.convertFromGraphicalToMathematicalSpace(
              Vector(e.clientX, e.clientY)
            )
            modelProxy.dispatchCB(MoveVertex(id, newPosition))
          }
          case None => {
            modelProxy.dispatchCB(NoAction)
          }
        }
      }

    private def handlePairsFromGeodesics(handles: SortedMap[Int, Complex[Double]], geodesics:List[(Int,Int)])
      = geodesics.map {g => ((g._1, handles(g._1)), (g._2, handles(g._2)))}

  }

  val component = ScalaComponent.builder[Props]("Canvas")
    .initialState(State(None))
    .renderBackend[Backend]
    .build

  def apply(modelProxy: ModelProxy[Root]) = component(Props(modelProxy))

}


object BoundaryCircle {
  case class Props(converter: Converter)

  class Backend(bs: BackendScope[Props, Unit]) {

    def render(props: Props) = {
      val converter: Converter = props.converter
      val centre = converter.convertFromMathematicalToGraphicalSpace(Complex(0.0, 0.0))
      val radius = converter.scaleFromMathematicalToGraphicalSpace(1.0)
      <.circle(
        ^.id := "boundary-circle",
        ^.cx := centre.x,
        ^.cy := centre.y,
        ^.r := radius,
        ^.stroke := "#000000",
        ^.fill := "none",
      )
    }
  }

  val component = ScalaComponent.builder[Props]("BoundaryCircle")
    .renderBackend[Backend]
    .build

  def apply(converter: Converter) = component(Props(converter))
}


object VertexHandle {
  case class Props(
    modelProxy: ModelProxy[Root],
    position: Vector,
    visibility: Visibility,
    handleMouseDown: Int => Callback,
    handleMouseUp: Callback,
    handleMouseMove: ReactMouseEventFromInput => Callback,
    key: Int,
  )

  class Backend(bs: BackendScope[Props, Unit]) {

    def render(props: Props) = {
      val position = props.position
      <.circle(
        ^.`class` := {
          props.visibility match {
            case Hide => "handle hide-away"
            case Show => "handle"
          }
        },
        ^.cx := position.x.toString,
        ^.cy := position.y.toString,
        ^.r := "4",
        ^.stroke := "none",
        ^.fill := "red",
        onMouseDown --> props.handleMouseDown(props.key),
        onMouseUp --> props.handleMouseUp,
        onClick ==> handleClick(props.modelProxy, props.key),
        onMouseMove ==> props.handleMouseMove,
      )
    }

    def handleClick(modelProxy: ModelProxy[Root], id: Int)(event: ReactMouseEventFromInput) = {
      modelProxy.dispatchCB({
        if (event.detail == 2) {
          VertexDoubleClick(id)
        } else if (event.detail == 3) {
          VertexTripleClick(id)
        } else  {
          NoAction
        }
      })
    }
  }

  val component = ScalaComponent.builder[Props]("VertexHandle")
    .renderBackend[Backend]
    .build

  def apply(
    modelProxy: ModelProxy[Root],
    position: Vector,
    visibility: Visibility,
    handleMouseDown: Int => Callback,
    handleMouseUp: Callback,
    handleMouseMove: ReactMouseEventFromInput => Callback,
    key: Int,
  ) = component.withKey(key)(
    Props(modelProxy, position, visibility, handleMouseDown, handleMouseUp, handleMouseMove, key)
  )
}


object GeodesicView {
  case class Props(startPoint: Complex[Double], endPoint: Complex[Double], transformation: MoebiusTransformation, converter: Converter)

  class Backend(bs: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      <.path(
        ^.stroke := "black",
        ^.fill := "none",
        ^.d := props.converter.convertCurveToSvg(
          props.transformation transform Geodesic(props.startPoint, props.endPoint, SpaceType.PoincareDisc) asCurve
        )
      )
    }
  }

  implicit val complexReuse = Reusability.byRef[Complex[Double]]
  implicit val mtReuse = Reusability.byRef[MoebiusTransformation]
  implicit val converter = Reusability.byRef[Converter]
  implicit val propsReuse = Reusability.derive[Props]

  val component = ScalaComponent.builder[Props]("GeodesicView")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(startPoint: (Int, Complex[Double]), endPoint: (Int, Complex[Double]), transformation: MoebiusTransformation, converter: Converter) = {
    val key = "%s-%s-%s".format(startPoint._1, endPoint._1, transformation.toString)
    component.withKey(key)(Props(startPoint._2, endPoint._2, transformation, converter))
  }
}


object GroupSelector {
  case class Props(
    modelProxy: ModelProxy[Root],
    group: Group,
    converter: Converter,
    visibility: Visibility
  )

  class Backend(bs: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      val centre = props.converter.convertFromMathematicalToGraphicalSpace(Complex(0.95, 0.95))
      val radius = props.converter.scaleFromMathematicalToGraphicalSpace(0.04)
      <.circle(
        ^.`class` := {
          props.visibility match {
            case Hide => "group-selector hide-away"
            case Show => "group-selector"
          }
        },
        ^.stroke := "black",
        ^.fill := {if (props.group == Group.trivialGroup) "white" else "black"},
        ^.cx := centre.x,
        ^.cy := centre.y,
        ^.r := radius,
        onClick --> handleClick(props.modelProxy, props.group),
      )
    }

    private def handleClick(modelProxy: ModelProxy[Root], group: Group)() = {
      modelProxy.dispatchCB(
        SelectGroup(if (group == Group.trivialGroup) Group.torsionFreeGroup(3) else Group.trivialGroup)
      )
    }
  }

  val component = ScalaComponent.builder[Props]("GroupSelector")
    .renderBackend[Backend]
    .build

  def apply(modelProxy: ModelProxy[Root], group: Group, converter: Converter, visibility: Visibility)
    = component(Props(modelProxy, group, converter, visibility))
}


object CircleLimitApp {

  val svgElementId = "main-display"

  val connection = AppCircuit.connect(AppCircuit.zoom(identity))

  def main(args: Array[String]): Unit = {
    setUpResizeHandler()
    setUpFading()
    resize()
    connection(p => Canvas(p)).renderIntoDOM(document.getElementById(svgElementId))
  }

  private def setUpResizeHandler() = {
    window.addEventListener("resize", (event: js.Dynamic) => resize() )
  }

  private def resize() = {
      val svgElement = document.getElementById(svgElementId)
      val svgWidth = svgElement.getBoundingClientRect().width.toDouble
      val svgHeight = svgElement.getBoundingClientRect().height.toDouble
      AppCircuit(Resize(svgWidth, svgHeight))
  }
  
  private def setUpFading() = {
    def makeDisapear() {
      AppCircuit(HideControls)
    }
    var t = window.setTimeout(() => makeDisapear, 3000)
    def resetTimer() {
      AppCircuit(ShowControls)
      window.clearTimeout(t)
      t = window.setTimeout(() => makeDisapear, 3000)
    }
    document.onmousemove = (event: MouseEvent) => resetTimer
  }
}
