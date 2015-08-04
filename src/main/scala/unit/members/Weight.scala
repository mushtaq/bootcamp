package unit.members

class WeightMeasure extends ScaledMeasure {
  type U = WeightUnit

  class WeightUnit(val scale: Double, val name: String) extends ScaledUnit {
    def apply(magnitude: Double) = new ScaledQuantity(magnitude, this)
  }
}

object Weight extends WeightMeasure {
  object Gram extends WeightUnit(1, "Gram")
  object Kilogram extends WeightUnit(1000, "Kilogram")
  object Ton extends WeightUnit(100000, "Ton")
}
