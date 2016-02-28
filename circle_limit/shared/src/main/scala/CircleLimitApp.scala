package circle_limit

import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.DynamicImplicits.number2dynamic
import org.scalajs.dom.html
import spire.implicits._

import circle_limit.maths.{
  Arc,
  Line
}
import circle_limit.maths.Imaginary.i
import circle_limit.graphics.ArcPlotter

@JSExport
object CircleLimitApp {
  @JSExport
  def main(movie: html.Div): Unit = {

    val transform = ArcPlotter.convertFromMathematicalToGraphicalSpace(
      (-1.1, -1.1),
      2.2,
      2.2,
      (0.0, 0.0),
      500, //magic number
      400, //magic number
      _ : (Double, Double)
    )
    val inputList = List(
      Arc(-1.0+0.0*i, 0.0-1.0*i, 0.0+0.0*i),
      Line(0.0+1.0*i, 1.0+0.0*i),
      Arc(-1.0+1.0*i, 1.0+1.0*i, 0.0+1.0*i)
    )
    val curveString = ArcPlotter.createConstructorStringFromList(
      inputList,
      transform
    )

    val document = js.Dynamic.global.document
    val playground = document.getElementById("movie")

    val newP = document.createElement("p")
    newP.innerHTML = "Hello world! <i>-- DOM</i>"
    playground.appendChild(newP)
    println("Seems to be working")
    val bonsai = js.Dynamic.global.bonsai
    bonsai.run(
      movie,
      js.Dynamic.literal(
        code = curveString,
        width = 500,
        height = 400 
      )
    )
  }
}
