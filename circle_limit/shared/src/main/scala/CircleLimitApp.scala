package circle_limit

import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.DynamicImplicits.number2dynamic
import org.scalajs.dom.html

@JSExport
object CircleLimitApp {
  @JSExport
  def main(movie: html.Div): Unit = {
    val document = js.Dynamic.global.document
    val playground = document.getElementById("movie")

    val newP = document.createElement("p")
    newP.innerHTML = "Hello world! <i>-- DOM</i>"
    playground.appendChild(newP)
    println("Seems to be working")
    val bonsai = js.Dynamic.global.bonsai
    val jsFun = () => {
      23
    }
    bonsai.run(
      movie,
      js.Dynamic.literal(
        code = jsFun,
        width = 500,
        height = 400 
      )
    )
    //println(rect)
    //val renderer = canvas.getContext("2d")
      //.asInstanceOf[dom.CanvasRenderingContext2D]
    //canvas.width = canvas.parentElement.clientWidth
    //canvas.height = canvas.parentElement.clientHeight
    //renderer.strokeStyle = "#EE5834"
    //renderer.fillRect(200, 200, 10, 10)
    //renderer.moveTo(0,0)
    //renderer.lineTo(100, 100)
    //renderer.stroke()
    //renderer.stroke()
    //println(canvas.width)
    //println(canvas.height)
    //println("is working")

    //var pointA = (0.0, 0.0)
    //var pointB = (0.0, 0.0)
    //canvas.onmousedown = (e: dom.MouseEvent) => {
      //pointA = (e.clientX, e.clientY)
      //println(pointA)
    //}
    //canvas.onmouseup = (e: dom.MouseEvent) => {
      //pointB = (e.clientX, e.clientY)
      //val radius = (pointB._1 - pointA._1) + (pointB._2 - pointA._2)
      //renderer.arc(pointA._1, pointA._2, radius, 0, 2*3.14)
      //renderer.stroke()
    //}
  }
}
