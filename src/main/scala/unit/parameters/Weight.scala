package unit.parameters

class WeightUnit(val scale: Double, val name: String) extends ScaledUnit[WeightUnit, ScaledQuantity[WeightUnit]] {
  def apply(magnitude: Double) = new ScaledQuantity(magnitude, this)
  def canBeConvertedTo(that: Unit[_, _]) = that.isInstanceOf[WeightUnit]
}

object Weight {
  object Gram extends WeightUnit(1, "Gram")
  object Kilogram extends WeightUnit(1000, "Kilogram")
  object Ton extends WeightUnit(100000, "Ton")
}
