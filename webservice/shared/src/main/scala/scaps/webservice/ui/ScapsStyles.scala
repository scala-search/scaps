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
      paddingTop := 90.px,

      dt(
        paddingTop := 20.px))

    def modulesBar = cls(
      marginTop := 50.px,
      minHeight := 0.px,

      ul(
        marginTop := 3.px,
        marginBottom := 5.px))

    def typeParameter = cls(
      color := "#999999")
  }
}
