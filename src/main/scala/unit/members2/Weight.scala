package unit.members2

class Weight extends ScaledMeasure {
  type U = Unit

  class Unit(val scale: Double, val name: String) extends super.Unit {
    def canBeConvertedTo(that: Measure#Unit) = that.isInstanceOf[Unit]
  }
}

object Weight extends Weight {
  object Gram extends Unit(1, "Gram")
  object Kilogram extends Unit(1000, "Kilogram")
  object Ton extends Unit(100000, "Ton")
}
