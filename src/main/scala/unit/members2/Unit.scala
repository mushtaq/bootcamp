package unit.members2

trait Measure {
  type U <: Unit
  type Q <: Quantity

  trait Unit {
    def name: String
    def apply(magnitude: Double): Q
    def canBeConvertedTo(that: Measure#Unit): Boolean
    def convertToBaseUnit(magnitude: Double): Double
    def convertFromBaseUnit(magnitude: Double): Double

    override def equals(that: Any) = that match {
      case x: Measure#Unit => canBeConvertedTo(x)
      case _               => false
    }
  }
  
  trait Quantity {
    def magnitude: Double
    def unit: U

    def magnitudeInBaseUnit = unit.convertToBaseUnit(magnitude)
    def isEqualTo(that: Q) = this.magnitudeInBaseUnit == that.magnitudeInBaseUnit
    def in(thatUnit: U) = thatUnit(thatUnit.convertFromBaseUnit(magnitudeInBaseUnit))

    override def equals(that: Any) = that match {
      case x: Q => unit == x.unit && isEqualTo(x)
      case _    => false
    }

    override def toString = s"$magnitude ${unit.name}"
  }
}

trait ScaledMeasure extends Measure {
  trait Unit extends super.Unit {
    def scale: Double

    def convertToBaseUnit(magnitude: Double) = magnitude * scale
    def convertFromBaseUnit(magnitude: Double) = magnitude / scale
  }
  
  trait Quantity extends super.Quantity {
    def +(that: Q): Q = unit(unit.convertFromBaseUnit(this.magnitudeInBaseUnit + that.magnitudeInBaseUnit))
  }
}

class LengthMeasure extends ScaledMeasure {
  type U = Unit
  type Q = Quantity

  class Unit(val scale: Double, val name: String) extends super.Unit {
    def canBeConvertedTo(that: Measure#Unit) = that.isInstanceOf[Unit]
    def apply(magnitude: Double) = new Quantity(magnitude, this)
  }
  
  class Quantity(val magnitude: Double, val unit: Unit) extends super.Quantity
}

object Length extends LengthMeasure {
  object Inches extends Unit(1, "Inches")
  object Feet extends Unit(12, "Feet")
  object Yards extends Unit(36, "Yard")
}

class WeightMeasure extends ScaledMeasure {
  type U = Unit
  type Q = Quantity

  class Unit(val scale: Double, val name: String) extends super.Unit {
    def canBeConvertedTo(that: Measure#Unit) = that.isInstanceOf[Unit]
    def apply(magnitude: Double) = new Quantity(magnitude, this)
  }

  class Quantity(val magnitude: Double, val unit: Unit) extends super.Quantity
}

object Weight extends WeightMeasure {
  object Gram extends Unit(1, "Gram")
  object Kilogram extends Unit(1000, "Kilogram")
  object Ton extends Unit(100000, "Ton")
}

class TemperatureMeasure extends Measure {
  type U = Unit
  type Q = Quantity

  abstract class Unit(val name: String) extends super.Unit {
    def canBeConvertedTo(that: Measure#Unit) = that.isInstanceOf[Unit]
    def apply(magnitude: Double) = new Quantity(magnitude, this)
  }

  class Quantity(val magnitude: Double, val unit: Unit) extends super.Quantity
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
