package scaps.webservice.ui

import scalatags.generic.Bundle
import scalatags.stylesheet.Sheet
import scalatags.stylesheet.CascadingStyleSheet

trait ScapsStyles[Builder, Output <: FragT, FragT] {
  val bundle: Bundle[Builder, Output, FragT]
  import bundle._
  import bundle.all._

  val ScapsStyle = Sheet[ScapsStyle]
  trait ScapsStyle extends CascadingStyleSheet {
    def world = cls(
      paddingTop := 70.px,

      dt(
        paddingTop := 20.px))

    def typeParameter = cls(
      color := "#999999")
  }
}
