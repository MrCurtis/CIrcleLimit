package circle_limit.graphics

import diode.{Circuit, RootModelRW}
import diode.react.ReactConnector


object AppCircuit extends Circuit[Root] with ReactConnector[Root]{

  def initialModel = Root(
    Converter(
      Box(-1.05, -1.05, 2.10, 2.10),
      Box(0.0, 0.0, 100, 100)
    ),
  )

  override val actionHandler: HandlerFunction = composeHandlers (
    new ConverterHandler(zoomTo(_.converter)),
    new GeometryHandler(zoomTo(_.geometry)),
    new GroupHandler(zoomTo(_.group)),
    new FadingHandler(zoomTo(_.visibility))
  )

}
