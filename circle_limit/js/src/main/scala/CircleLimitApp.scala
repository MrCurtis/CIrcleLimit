package circle_limit

import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.DynamicImplicits.number2dynamic
import org.scalajs.dom.html
import spire.math.Complex
import spire.implicits._
import circle_limit.maths.CircleImplicits._

import circle_limit.maths.{
  Arc,
  Geodesic,
  Group,
  Line,
  MoebiusTransformation,
  SpaceType
}
import circle_limit.maths.Imaginary.i
import circle_limit.graphics.ArcPlotter

@JSExport
object CircleLimitApp {
  @JSExport
  def main(movie: html.Div): Unit = {

    val displayWidth = 1500
    val displayHeight = 1000
    val transform = ArcPlotter.convertFromMathematicalToGraphicalSpace(
      (-1.0, -1.0),
      2.0,
      2.0,
      (0.0, 0.0),
      displayWidth,
      displayHeight,
      _ : (Double, Double)
    )

    val identity = MoebiusTransformation(1.0, 0.0, 0.0, 1.0)
    val transform1 = MoebiusTransformation(3.0, 2.0+1.0*i, 2.0-1.0*i, 3.0)
    val transform1Inverse = MoebiusTransformation(3.0, -2.0-1.0*i, -2.0+1.0*i, 3.0)
    val transform2 = MoebiusTransformation(3.0, 2.0-1.0*i, 2.0+1.0*i, 3.0)
    val transform2Inverse = MoebiusTransformation(3.0, -2.0+1.0*i, -2.0-1.0*i, 3.0)
    val wordLength = 6
    val group = Group(List(transform1, transform2), wordLength)
    val geodesic1 = Geodesic(Complex[Double](-1.0, 0.0), Complex[Double](0.0, 1.0), SpaceType.PoincareDisc)
    val geodesic2 = Geodesic(Complex[Double](-1.0, 0.0), Complex[Double](0.0, -1.0), SpaceType.PoincareDisc)
    val geodesics = Set(geodesic1, geodesic2)

    val allGeodesics = group.getImagesOfGeodesics(geodesics)
    val allCurves = allGeodesics map (g => g.asCurve)

    val inputList = allCurves.toList
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
        width = displayWidth,
        height = displayHeight 
      )
    )
  }
}
