package circle_limit

import scala.scalajs.js

import org.scalajs.dom.window
import org.scalajs.dom.document
import org.scalajs.dom.raw.MouseEvent

import circle_limit.graphics.AppCircuit
import circle_limit.graphics.{
  Resize,
  HideControls,
  ShowControls,
  Canvas,
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
