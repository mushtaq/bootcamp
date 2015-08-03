package unit

trait Unit {
  type U <: Unit
  type Q = U#Quantity
  val self: U
  def name: String
  def apply(magnitude: Double): Q
  def isConvertible(that: Unit): Boolean
  def convertToBase(magnitude: Double): Double
  def convertFromBase(magnitude: Double): Double

  override def equals(that: Any) = that match {
    case x: Unit => isConvertible(x)
    case _       => false
  }

  trait Quantity {
    def magnitude: Double
    def unit = self

    def magnitudeInBaseUnit = convertToBase(magnitude)

    def isEqualTo(that: Q) = this.magnitudeInBaseUnit == that.magnitudeInBaseUnit

    override def equals(that: Any) = that match {
      case x: Q => unit == x.unit && isEqualTo(x)
      case _    => false
    }

    def in(thatUnit: U) = thatUnit(thatUnit.convertFromBase(magnitudeInBaseUnit))
    override def toString = s"$magnitude $name"
  }
}

trait ScaledUnit extends Unit {
  def scale: Double

  def convertToBase(magnitude: Double) = magnitude * scale
  def convertFromBase(magnitude: Double) = magnitude / scale

  trait Quantity extends super.Quantity {
    def +(that: Q): Q = apply(convertFromBase(this.magnitudeInBaseUnit + that.magnitudeInBaseUnit))
  }
}

class Length(val scale: Double, val name: String) extends ScaledUnit {
  type U = Length
  val self = this

  def isConvertible(that: Unit) = that.isInstanceOf[U]
  def apply(magnitude: Double) = new Quantity(magnitude)
  class Quantity(val magnitude: Double) extends super.Quantity
}

object Length {
  object Inches extends Length(1, "Inches")
  object Feet extends Length(12, "Feet")
  object Yards extends Length(36, "Yard")
}

class Weight(val scale: Double, val name: String) extends ScaledUnit {
  type U = Weight
  val self = this

  def isConvertible(that: Unit) = that.isInstanceOf[U]
  def apply(magnitude: Double) = new Quantity(magnitude)
  class Quantity(val magnitude: Double) extends super.Quantity
}

object Weight {
  object Gram extends Weight(1, "Gram")
  object Kilogram extends Weight(1000, "Kilogram")
  object Ton extends Weight(100000, "Ton")
}

abstract class Temperature(val name: String) extends Unit {
  type U = Temperature
  def isConvertible(that: Unit) = that.isInstanceOf[U]
  def apply(magnitude: Double) = new Quantity(magnitude)

  class Quantity(val magnitude: Double) extends super.Quantity
}

object Temperature {
  object Celsius extends Temperature("Celsius") {
    val self = this

    def convertToBase(magnitude: Double) = magnitude
    def convertFromBase(magnitude: Double) = magnitude
  }

  object Fahrenheit extends Temperature("Fahrenheit") {
    val self = this

    def convertToBase(magnitude: Double) = (magnitude - 32) * 5 / 9
    def convertFromBase(magnitude: Double) = (magnitude * 9 / 5) + 32
  }
}
