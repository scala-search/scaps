package scala.tools.apiSearch.settings

object Instances {
  implicit object IndexSettingsOrdering extends Ordering[IndexSettings] {
    override def compare(x: IndexSettings, y: IndexSettings) =
      implicitly[Ordering[Double]].compare(x.lengthNormWeight, y.lengthNormWeight)
  }

  implicit object SettingsOrdering extends Ordering[Settings] {
    override def compare(x: Settings, y: Settings) =
      implicitly[Ordering[IndexSettings]].compare(x.index, y.index)
  }
}
