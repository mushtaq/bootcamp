package unit.members2

object Length extends LengthMeasure {
  object Inches extends Unit(1, "Inches")
  object Feet extends Unit(12, "Feet")
  object Yards extends Unit(36, "Yard")
}

object Weight extends WeightMeasure {
  object Gram extends Unit(1, "Gram")
  object Kilogram extends Unit(1000, "Kilogram")
  object Ton extends Unit(100000, "Ton")
}

object Temperature extends TemperatureMeasure {
  object Celsius extends Unit("Celsius") {
    def convertToBaseUnit(magnitude: Double) = magnitude
    def convertFromBaseUnit(magnitude: Double) = magnitude
  }

  object Fahrenheit extends Unit("Fahrenheit") {
    def convertToBaseUnit(magnitude: Double) = (magnitude - 32) * 5 / 9
    def convertFromBaseUnit(magnitude: Double) = (magnitude * 9 / 5) + 32
  }
}
