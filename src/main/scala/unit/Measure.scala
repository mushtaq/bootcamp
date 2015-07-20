package unit

trait Measure {
  type Q <: Measure#Quantity
  def name: String
  def apply(magnitude: Double): Q
  def isQ(that: Any): Boolean

  trait Quantity {
    def magnitude: Double
    def magnitudeInBaseUnit: Double
    
    def isEqualTo(that: Q) = this.magnitudeInBaseUnit == that.magnitudeInBaseUnit
    override def equals(that: Any) = isQ(that) && isEqualTo(that.asInstanceOf[Q])
    override def toString = s"$magnitude $name"
  }
}

trait ScaledMeasure extends Measure {
  def scale: Double

  trait Quantity extends super.Quantity {
    def magnitudeInBaseUnit = scale * magnitude
    def +(that: Q): Q = apply((this.magnitudeInBaseUnit + that.magnitudeInBaseUnit) / scale)
  }
}

class Length(val scale: Double, val name: String) extends ScaledMeasure {
  type Q = Length#Quantity
  def isQ(that: Any) = that.isInstanceOf[Q]
  def apply(magnitude: Double) = new Quantity(magnitude)
  class Quantity(val magnitude: Double) extends super.Quantity
}

object Length {
  object Inches extends Length(1, "Inches")
  object Feet extends Length(12, "Feet")
  object Yards extends Length(36, "Yard")
}

class Weight(val scale: Double, val name: String) extends ScaledMeasure {
  type Q = Weight#Quantity
  def isQ(that: Any) = that.isInstanceOf[Q]
  def apply(magnitude: Double) = new Quantity(magnitude)
  class Quantity(val magnitude: Double) extends super.Quantity
}

object Weight {
  object Gram extends Weight(1, "Gram")
  object Kilogram extends Weight(1000, "Kilogram")
  object Ton extends Weight(100000, "Ton")
}

abstract class Temperature(val name: String) extends Measure {
  def convertToBase(magnitude: Double): Double

  type Q = Temperature#Quantity
  def isQ(that: Any) = that.isInstanceOf[Q]
  def apply(magnitude: Double) = new Quantity(magnitude)

  class Quantity(val magnitude: Double) extends super.Quantity {
    def magnitudeInBaseUnit = convertToBase(magnitude)
  }
}

object Temperature {
  object Celsius extends Temperature("Celsius") {
    def convertToBase(magnitude: Double) = magnitude
  }

  object Fahrenheit extends Temperature("Fahrenheit") {
    def convertToBase(magnitude: Double) = (magnitude - 32) * 5 / 9
  }
}
