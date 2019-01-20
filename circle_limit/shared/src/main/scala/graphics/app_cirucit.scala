package circle_limit.graphics

import diode.{Circuit, RootModelRW}


object AppCircuit extends Circuit[Root] {

  def initialModel = Root(
    Converter(
      Box(-1.05, -1.05, 2.10, 2.10),
      Box(0.0, 0.0, 100, 100)
    ),
  )

  override val actionHandler: HandlerFunction = composeHandlers (
    new ConverterHandler(zoomTo(_.converter)),
    new GeometryHandler(zoomTo(_.geometry))
  )

}
