package unit.members

class Temperature extends Measure {
  type U = TemperatureUnit
  type Q = TemperatureQuantity

  abstract class TemperatureUnit(val name: String) extends Unit {
    def apply(magnitude: Double) = new TemperatureQuantity(magnitude, this)
  }

  class TemperatureQuantity(val magnitude: Double, val unit: TemperatureUnit) extends Quantity
}

object Temperature extends Temperature {
  object Celsius extends TemperatureUnit("Celsius") {
    def convertToBaseUnit(magnitude: Double) = magnitude
    def convertFromBaseUnit(magnitude: Double) = magnitude
  }

  object Fahrenheit extends TemperatureUnit("Fahrenheit") {
    def convertToBaseUnit(magnitude: Double) = (magnitude - 32) * 5 / 9
    def convertFromBaseUnit(magnitude: Double) = (magnitude * 9 / 5) + 32
  }
}
