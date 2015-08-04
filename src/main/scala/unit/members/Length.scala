package unit.members

class Length extends ScaledMeasure {
  type U = LengthUnit

  class LengthUnit(val scale: Double, val name: String) extends ScaledUnit {
    def apply(magnitude: Double) = new ScaledQuantity(magnitude, this)
  }
}

object Length extends Length {
  object Inches extends LengthUnit(1, "Inches")
  object Feet extends LengthUnit(12, "Feet")
  object Yards extends LengthUnit(36, "Yard")
}
