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
    bonsai.run(
      movie,
      js.Dynamic.literal(
        code = "new Rect(10, 10, 100, 100).addTo(stage).attr('fillColor', 'green'); new Rect(10, 120, 100, 100).addTo(stage).attr('fillColor', 'red'); new Rect(10, 230, 100, 100).addTo(stage).attr('fillColor', 'blue');",
        width = 500,
        height = 400 
      )
    )
  }
}
