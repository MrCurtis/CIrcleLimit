package circle_limit

import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.DynamicImplicits.number2dynamic

import org.scalajs.dom.window
import org.scalajs.dom.document
import org.scalajs.dom.raw.MouseEvent
import spire.math.Complex
import spire.implicits._
import diode.{ModelRO, NoAction}
import diode.react.ModelProxy

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.svg_<^._
import japgolly.scalajs.react.vdom.html_<^.<.button
import japgolly.scalajs.react.vdom.html_<^.^.{key, onClick, onMouseUp, onMouseDown, onMouseMove}

import circle_limit.maths.CircleImplicits._
import circle_limit.maths.Imaginary.i
import circle_limit.maths.{Geodesic, SpaceType, Group}
import circle_limit.graphics.{Converter, Box, Vector}
import circle_limit.graphics.AppCircuit
import circle_limit.graphics.{
  Root,
  CanvasSingleClick,
  CanvasDoubleClick,
  MoveVertex,
  SelectGroup,
  VertexDoubleClick,
  VertexTripleClick,
  Resize
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
      def handleMouseUp() = {println("mouse up"); bs.setState(State(None))}
      def handleMouseMove(e: ReactMouseEventFromInput) = {
          println(state.vertexSelected)
          state.vertexSelected match {
            case Some(id) => {
              val newPosition = props.modelProxy().converter.convertFromGraphicalToMathematicalSpace(Vector(e.clientX, e.clientY))
              props.modelProxy.dispatchCB(MoveVertex(1, newPosition))
            }
            case None => {
              props.modelProxy.dispatchCB(NoAction)
            }
          }
        }

      val vertices = model.geometry.handles
      val toGraphical = model.converter.convertFromMathematicalToGraphicalSpace(_)
      val vertexElements = vertices.map(
        vertex => {
          VertexHandle(toGraphical(vertex.position), props.modelProxy, handleMouseDown, handleMouseUp, key=Some(vertex.id))
        }
      )

      <.svg(
        <.rect(
          ^.id := "canvas",
          ^.height := "100%",
          ^.width := "100%",
          ^.fill := "white",
          onClick ==> handleClick,
          onMouseMove ==> handleMouseMove,
          onMouseUp --> handleMouseUp
        ),
        BoundaryCircle(model.converter),
        vertexElements.toTagMod,
      )
    }


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
    position: Vector,
    modelProxy: ModelProxy[Root],
    handleMouseDown: Int => Callback,
    handleMouseUp: Callback,
  )

  class Backend(bs: BackendScope[Props, Unit]) {

    def render(props: Props) = {
      val position = props.position
      <.circle(
        ^.`class` := "handle control",
        ^.cx := position.x.toString,
        ^.cy := position.y.toString,
        ^.r := "4",
        ^.stroke := "none",
        ^.fill := "red",
        onMouseDown --> props.handleMouseDown(1),
        onMouseUp --> props.handleMouseUp
      )
    }
  }

  val component = ScalaComponent.builder[Props]("VertexHandle")
    .renderBackend[Backend]
    .build

  def apply(
    position: Vector,
    modelProxy: ModelProxy[Root],
    handleMouseDown: Int => Callback,
    handleMouseUp: Callback,
    key: Option[Int]=None
  ) = {
    key match {
      case Some(x) => component.withKey(x)(Props(position, modelProxy, handleMouseDown, handleMouseUp))
      case None => component(Props(position, modelProxy, handleMouseDown, handleMouseUp))
    }
  }
}


object CircleLimitApp {

  val svgElementId = "main-display"

  val connection = AppCircuit.connect(AppCircuit.zoom(identity))

  def main(args: Array[String]): Unit = {
    setUpResizeHandler()
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
}

/*@JSExport*/
//object CircleLimitApp {

  //val svgElementId = "main-display"

  //@JSExport
  //def main(): Unit = {

    //var svgElement = js.Dynamic.global.Snap().attr(js.Dictionary("id" -> svgElementId))
    //setUpFading(svgElement)
    //setUpResizeHandler()
    //AppCircuit.subscribe(AppCircuit.zoom(identity))(root => render(root, svgElement))
    //resize()
  //}

  //private var cachedRoot: Option[Root] = None

  //def render(root: ModelRO[Root], svgElement: js.Dynamic) = {
    //val handles = root.value.geometry.handles
    //val converter = root.value.converter
    //val geodesics = root.value.geometry.geodesics
    //val group = root.value.group
    //setUpCanvasEventHandlers(svgElement, converter)
    //svgElement.selectAll("[class~=handle]").forEach((el: js.Dynamic) => el.remove())
    //val convert = converter.convertFromMathematicalToGraphicalSpace(_)
    //createBoundaryCircle(svgElement, converter)
    //svgElement.selectAll("[id=iteration-button]").forEach((el: js.Dynamic) => el.remove())
    //val centre = converter.convertFromMathematicalToGraphicalSpace(Complex(0.95, 0.95))
    //val radius = converter.scaleFromMathematicalToGraphicalSpace(0.04)
    //val el = svgElement.circle(centre.x, centre.y, radius).attr(
      //js.Dictionary(
        //"stroke" -> "black",
        //"fill" -> {if (group == Group.trivialGroup) "white" else "black"},
        //"id" -> "iteration-button",
        //"class" -> "control"
      //)
    //)
    //el.click(
      //(event: js.Dynamic) => {
        //AppCircuit(
          //SelectGroup(if (group == Group.trivialGroup) Group.torsionFreeGroup(3) else Group.trivialGroup)
        //)
      //}
    //)
    //val (createdGeodesics, changedGeodesics, deletedGeodesics) = getGeodesicsDiff(root.value, cachedRoot)
    //(changedGeodesics.map(_._1) union deletedGeodesics).foreach(
      //x => {
        //svgElement.selectAll("[class~=%s]".format(x)).forEach((el: js.Dynamic) => el.remove())
      //}
    //)
    //(createdGeodesics union changedGeodesics).foreach(
      //g => {
        //val geodesic = Geodesic(g._2._1, g._2._2, SpaceType.PoincareDisc)
        //val images = group.getImagesOfGeodesic(geodesic)
        //images.foreach(
          //(image: Geodesic) => {
            //svgElement.path(converter.convertCurveToSvg(image.asCurve)).attr(
              //js.Dictionary("stroke" -> "black", "fill" -> "none",  "class" -> "%s".format(g._1)))
          //}
        //)
      //}
    //)
    //handles.foreach(
      //h => {
        //val position = convert(h.position)
        //val el = svgElement.circle(position.x, position.y, 4).attr(
          //js.Dictionary("fill" -> "red", "class" -> "handle control", "id" -> h.id)
        //)
        //var tempStoreX = "0.0"
        //var tempStoreY = "0.0"
        //el.drag(
          //((event: js.Dynamic, dX: Double, dY: Double, posX: Double, posY: Double) =>
            //{
              //val cx = (tempStoreX.toDouble+dX.toDouble)
              //val cy = (tempStoreY.toDouble+dY.toDouble)
              //val elementId = el.attr("id").toString
              //val posMathematical = converter.convertFromGraphicalToMathematicalSpace(
                //Vector(cx.toDouble,cy.toDouble))
              //AppCircuit(MoveVertex(elementId.toInt, posMathematical))
            //}): js.ThisFunction,
          //((event: js.Dynamic, x: Double, y: Double) =>
            //{
              //tempStoreX = el.attr("cx").toString
              //tempStoreY = el.attr("cy").toString
            //}): js.ThisFunction,
          //((event: js.Dynamic) =>
            //{
            //}): js.ThisFunction
        //)
        //el.click(
          //(event: js.Dynamic) => {
            //event.stopPropagation()
            //if (event.detail == 2) {
              //val elementId = el.attr("id").toString.toInt
              //AppCircuit(VertexDoubleClick(elementId))
            //}
            //if (event.detail == 3){
              //val elementId = el.attr("id").toString.toInt
              //AppCircuit(VertexTripleClick(elementId))
            //}
          //}
        //)
      //}
    //)
    //cachedRoot = Some(root.value)
  //}

  //private def getGeodesicsDiff(root: Root, cachedRoot: Option[Root]): (
    //Set[(String, (Complex[Double], Complex[Double]))],
    //Set[(String, (Complex[Double], Complex[Double]))],
    //Set[String]
  //) =
    //cachedRoot match {
      //case None => {
        //val handles = root.geometry.handles
        //val geodesics = root.geometry.geodesics
        //(
        //geodesics.map(
          //g => (
            //"join%dwith%d".format(g._1, g._2),
            //(handles.filter(_.id == g._1).head.position, handles.filter(_.id == g._2).head.position)
          //)
        //),
        //Set(),
        //Set()
       //)
      //}
      //case Some(cached) if (cached.converter != root.converter || cached.group != root.group) => {
        //val handles = root.geometry.handles
        //val geodesics = root.geometry.geodesics
        //(
        //Set(),
        //geodesics.map(
          //g => (
            //"join%dwith%d".format(g._1, g._2),
            //(handles.filter(_.id == g._1).head.position, handles.filter(_.id == g._2).head.position)
          //)
        //),
        //Set()
       //)
      //}
      //case Some(cached) => {
        //val handles = root.geometry.handles
        //val createdGeodesics = root.geometry.geodesics -- cached.geometry.geodesics
        //val deletedGeodesics = cached.geometry.geodesics -- root.geometry.geodesics
        //val changedVertexIds = (root.geometry.handles -- cached.geometry.handles).map(_.id)
        //val changedGeodesics = (root.geometry.geodesics intersect cached.geometry.geodesics).filter(
          //g => {(changedVertexIds contains g._1) || (changedVertexIds contains g._2)}
        //)
        //(
          //createdGeodesics.map(
            //g => (
              //"join%dwith%d".format(g._1, g._2),
              //(handles.filter(_.id == g._1).head.position, handles.filter(_.id == g._2).head.position)
            //)
          //),
          //changedGeodesics.map(
            //g => (
              //"join%dwith%d".format(g._1, g._2),
              //(handles.filter(_.id == g._1).head.position, handles.filter(_.id == g._2).head.position)
            //)
          //),
          //deletedGeodesics.map(
            //g => "join%dwith%d".format(g._1, g._2)
          //)
        //)
      //}
    //}

  //private def setUpFading(svgElement: js.Dynamic) = {
    //def makeDisapear() {
      //var elements = svgElement.selectAll("[class~=control]")
      //elements.forEach((el: js.Dynamic) => el.toggleClass("hide-away", true))
    //}
    //var t = window.setTimeout(() => makeDisapear, 3000)
    //def resetTimer() {
      //var elements = svgElement.selectAll("[class~=control]")
      //elements.forEach((el: js.Dynamic) => el.toggleClass("hide-away", false))
      //window.clearTimeout(t)
      //t = window.setTimeout(() => makeDisapear, 3000)
    //}
    //document.onmousemove = (event: MouseEvent) => resetTimer
  //}

  //private def setUpResizeHandler() = {
    //window.addEventListener("resize", (event: js.Dynamic) => resize() )
  //}

  //private def resize() = {
      //val svgElement = document.getElementById(svgElementId)
      //val svgWidth = svgElement.getBoundingClientRect().width.toDouble
      //val svgHeight = svgElement.getBoundingClientRect().height.toDouble
      //AppCircuit(Resize(svgWidth, svgHeight))
  //}

  //private def createBoundaryCircle(svgElement: js.Dynamic, converter: Converter) = {
    //svgElement.selectAll("[id=boundary-circle]").forEach((el: js.Dynamic) => el.remove())
    //val centre = converter.convertFromMathematicalToGraphicalSpace(Complex(0.0, 0.0))
    //val radius = converter.scaleFromMathematicalToGraphicalSpace(1.0)
    //svgElement.circle(centre.x, centre.y, radius).attr(
          //js.Dictionary("stroke" -> "black", "fill" -> "none", "id" -> "boundary-circle"))
  //}

  //private def setUpCanvasEventHandlers(svgElement: js.Dynamic, converter: Converter) = {
    //svgElement.unclick()
    //svgElement.click(
      //(event: js.Dynamic) => {
        //if (event.detail == 2) {
          //val posMathematical = converter.convertFromGraphicalToMathematicalSpace(
            //Vector(event.clientX.toString.toDouble, event.clientY.toString.toDouble))
          //AppCircuit(CanvasDoubleClick(posMathematical))
        //} else if (event.detail == 1){
          //val posMathematical = converter.convertFromGraphicalToMathematicalSpace(
            //Vector(event.clientX.toString.toDouble, event.clientY.toString.toDouble))
          //AppCircuit(CanvasSingleClick(posMathematical))
        //}
      //}
    //)
  //}
/*}*/
