package unit.parameters

class LengthUnit(val scale: Double, val name: String) extends ScaledUnit[LengthUnit, ScaledQuantity[LengthUnit]] {
  def apply(magnitude: Double) = new ScaledQuantity[LengthUnit](magnitude, this)
  def canBeConvertedTo(that: Unit[_, _]) = that.isInstanceOf[LengthUnit]
}

object Length {
  object Inches extends LengthUnit(1, "Inches")
  object Feet extends LengthUnit(12, "Feet")
  object Yards extends LengthUnit(36, "Yard")
}
