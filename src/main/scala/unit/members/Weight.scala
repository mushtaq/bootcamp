package unit.members

class Weight extends ScaledMeasure {
  type U = WeightUnit

  class WeightUnit(val scale: Double, val name: String) extends ScaledUnit {
    def apply(magnitude: Double) = new ScaledQuantity(magnitude, this)
  }
}

object Weight extends Weight {
  object Gram extends WeightUnit(1, "Gram")
  object Kilogram extends WeightUnit(1000, "Kilogram")
  object Ton extends WeightUnit(100000, "Ton")
}
