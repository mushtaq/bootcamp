package unit

trait Unit {
  type U <: Unit
  type Q = U#Quantity

  def name: String

  def apply(magnitude: Double): Q
  def canBeConvertedTo(that: Unit): Boolean

  def inBaseUnits(magnitude: Double): Double
  def fromBaseUnit(magnitude: Double): Double

  override def equals(that: Any) = that match {
    case x: Unit => canBeConvertedTo(x)
    case _       => false
  }

  trait Quantity {
    def magnitude: Double
    def unit: U

    def magnitudeInBaseUnit = inBaseUnits(magnitude)

    def isEqualTo(that: Q) = this.magnitudeInBaseUnit == that.magnitudeInBaseUnit

    override def equals(that: Any) = that match {
      case x: Q => unit == x.unit && isEqualTo(x)
      case _    => false
    }

    def in(thatUnit: U) = thatUnit(thatUnit.fromBaseUnit(magnitudeInBaseUnit))
    override def toString = s"$magnitude $name"
  }
}

trait ScaledUnit extends Unit {
  def scale: Double

  def inBaseUnits(magnitude: Double) = magnitude * scale
  def fromBaseUnit(magnitude: Double) = magnitude / scale

  trait Quantity extends super.Quantity {
    def +(that: Q): Q = apply(fromBaseUnit(this.magnitudeInBaseUnit + that.magnitudeInBaseUnit))
  }
}

class Length(val scale: Double, val name: String) extends ScaledUnit {
  type U = Length
  def canBeConvertedTo(that: Unit) = that.isInstanceOf[Length]
  def apply(magnitude: Double) = new Quantity(magnitude, this)
  class Quantity(val magnitude: Double, val unit: Length) extends super.Quantity
}

object Length {
  object Inches extends Length(1, "Inches")
  object Feet extends Length(12, "Feet")
  object Yards extends Length(36, "Yard")
}

class Weight(val scale: Double, val name: String) extends ScaledUnit {
  type U = Weight
  def canBeConvertedTo(that: Unit) = that.isInstanceOf[Weight]
  def apply(magnitude: Double) = new Quantity(magnitude, this)
  class Quantity(val magnitude: Double, val unit: Weight) extends super.Quantity
}

object Weight {
  object Gram extends Weight(1, "Gram")
  object Kilogram extends Weight(1000, "Kilogram")
  object Ton extends Weight(100000, "Ton")
}

abstract class Temperature(val name: String) extends Unit {
  type U = Temperature
  def canBeConvertedTo(that: Unit) = that.isInstanceOf[Temperature]
  def apply(magnitude: Double) = new Quantity(magnitude, this)
  class Quantity(val magnitude: Double, val unit: Temperature) extends super.Quantity
}

object Temperature {
  object Celsius extends Temperature("Celsius") {
    def inBaseUnits(magnitude: Double) = magnitude
    def fromBaseUnit(magnitude: Double) = magnitude
  }

  object Fahrenheit extends Temperature("Fahrenheit") {
    def inBaseUnits(magnitude: Double) = (magnitude - 32) * 5 / 9
    def fromBaseUnit(magnitude: Double) = (magnitude * 9 / 5) + 32
  }
}
