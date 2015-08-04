package unit.parameters

abstract class TemperatureUnit(val name: String) extends Unit[TemperatureUnit, TemperatureQuantity] {
  def apply(magnitude: Double) = new TemperatureQuantity(magnitude, this)
  def canBeConvertedTo(that: Unit[_, _]) = that.isInstanceOf[TemperatureUnit]
}

class TemperatureQuantity(val magnitude: Double, val unit: TemperatureUnit) extends Quantity[TemperatureUnit, TemperatureQuantity]

object Temperature {
  object Celsius extends TemperatureUnit("Celsius") {
    def convertToBaseUnit(magnitude: Double) = magnitude
    def convertFromBaseUnit(magnitude: Double) = magnitude
  }

  object Fahrenheit extends TemperatureUnit("Fahrenheit") {
    def convertToBaseUnit(magnitude: Double) = (magnitude - 32) * 5 / 9
    def convertFromBaseUnit(magnitude: Double) = (magnitude * 9 / 5) + 32
  }
}
