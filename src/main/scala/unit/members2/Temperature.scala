package unit.members2

class Temperature extends Measure {
  type U = Unit
  type Q = Quantity

  abstract class Unit(val name: String) extends super.Unit {
    def canBeConvertedTo(that: Measure#Unit) = that.isInstanceOf[Unit]
    def apply(magnitude: Double) = new Quantity(magnitude, this)
  }

  class Quantity(val magnitude: Double, val unit: Unit) extends super.Quantity
}

object Temperature extends Temperature {
  object Celsius extends Unit("Celsius") {
    def convertToBaseUnit(magnitude: Double) = magnitude
    def convertFromBaseUnit(magnitude: Double) = magnitude
  }

  object Fahrenheit extends Unit("Fahrenheit") {
    def convertToBaseUnit(magnitude: Double) = (magnitude - 32) * 5 / 9
    def convertFromBaseUnit(magnitude: Double) = (magnitude * 9 / 5) + 32
  }
}
