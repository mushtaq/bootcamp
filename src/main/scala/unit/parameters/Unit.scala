package unit.parameters

import unit.parameters

trait Quantity[U <: Unit[U, Q], Q <: Quantity[U, Q]] {
  def magnitude: Double
  def unit: U

  def magnitudeInBaseUnit = unit.convertToBaseUnit(magnitude)

  def isEqualTo(that: Q) = this.magnitudeInBaseUnit == that.magnitudeInBaseUnit

  override def equals(that: Any) = that match {
    case x: Q => unit == x.unit && isEqualTo(x)
    case _    => false
  }

  def in(thatUnit: U) = thatUnit(thatUnit.convertFromBaseUnit(magnitudeInBaseUnit))
  override def toString = s"$magnitude ${unit.name}"
}

trait Unit[U <: Unit[U, Q], Q <: Quantity[U, Q]] {
  def name: String

  def apply(magnitude: Double): Q
  def canBeConvertedTo(that: Unit[_, _]): Boolean

  def convertToBaseUnit(magnitude: Double): Double
  def convertFromBaseUnit(magnitude: Double): Double

  override def equals(that: Any) = that match {
    case x: Unit[_, _] => canBeConvertedTo(x)
    case _       => false
  }
}

trait ScaledQuantity[U <: Unit[U, Q], Q <: Quantity[U, Q]] extends Quantity[U, Q] {
  def +(that: Q): Q = unit(unit.convertFromBaseUnit(this.magnitudeInBaseUnit + that.magnitudeInBaseUnit))
}

trait ScaledUnit[U <: Unit[U, Q], Q <: Quantity[U, Q]] extends Unit[U, Q] {
  def scale: Double

  def convertToBaseUnit(magnitude: Double) = magnitude * scale
  def convertFromBaseUnit(magnitude: Double) = magnitude / scale
}


class Length(val scale: Double, val name: String) extends ScaledUnit[Length, Length.Quantity] {
  def canBeConvertedTo(that: Unit[_, _]) = that.isInstanceOf[Length]
  def apply(magnitude: Double) = new Length.Quantity(magnitude, this)
}

object Length {
  class Quantity(val magnitude: Double, val unit: Length) extends ScaledQuantity[Length, Quantity]

  object Inches extends Length(1, "Inches")
  object Feet extends Length(12, "Feet")
  object Yards extends Length(36, "Yard")
}

class Weight(val scale: Double, val name: String) extends ScaledUnit[Weight, Weight.Quantity] {
  def canBeConvertedTo(that: Unit[_, _]) = that.isInstanceOf[Weight]
  def apply(magnitude: Double) = new Weight.Quantity(magnitude, this)
}

object Weight {
  class Quantity(val magnitude: Double, val unit: Weight) extends ScaledQuantity[Weight, Quantity]

  object Gram extends Weight(1, "Gram")
  object Kilogram extends Weight(1000, "Kilogram")
  object Ton extends Weight(100000, "Ton")
}


abstract class Temperature(val name: String) extends Unit[Temperature, Temperature.Quantity] {
  def canBeConvertedTo(that: Unit[_, _]) = that.isInstanceOf[Temperature]
  def apply(magnitude: Double) = new Temperature.Quantity(magnitude, this)
}

object Temperature {

  class Quantity(val magnitude: Double, val unit: Temperature) extends parameters.Quantity[Temperature, Quantity]

  object Celsius extends Temperature("Celsius") {
    def convertToBaseUnit(magnitude: Double) = magnitude
    def convertFromBaseUnit(magnitude: Double) = magnitude
  }

  object Fahrenheit extends Temperature("Fahrenheit") {
    def convertToBaseUnit(magnitude: Double) = (magnitude - 32) * 5 / 9
    def convertFromBaseUnit(magnitude: Double) = (magnitude * 9 / 5) + 32
  }
}
