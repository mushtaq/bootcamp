package unit.members2

class Length extends ScaledMeasure {
  type U = Unit

  class Unit(val scale: Double, val name: String) extends super.Unit {
    def canBeConvertedTo(that: Measure#Unit) = that.isInstanceOf[Unit]
  }
}

object Length extends Length {
  object Inches extends Unit(1, "Inches")
  object Feet extends Unit(12, "Feet")
  object Yards extends Unit(36, "Yard")
}
