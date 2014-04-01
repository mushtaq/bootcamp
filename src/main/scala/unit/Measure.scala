package unit

trait Measure {
  type Q <: Measure#Quantity
  def name: String
  def apply(n: Double): Q
  def isComparableWith(that: Any): Boolean

  trait Quantity {
    def magnitude: Double
    def magnitudeInBaseUnit: Double
    
    def isEqualTo(that: Q) = this.magnitudeInBaseUnit == that.magnitudeInBaseUnit
    override def equals(that: Any) = isComparableWith(that) && isEqualTo(that.asInstanceOf[Q])
    override def toString = s"$magnitude $name"
  }
}

trait AddableMeasure extends Measure {
  def scale: Double
  
  trait Quantity extends super.Quantity {
    def magnitudeInBaseUnit = scale * magnitude
    def +(that: Q): Q = apply((this.magnitudeInBaseUnit + that.magnitudeInBaseUnit) / scale)
  }
}

class Length(val scale: Double, val name: String) extends AddableMeasure {
  type Q = Length#Quantity
  def isComparableWith(that: Any) = that.isInstanceOf[Q]
  def apply(n: Double) = new Quantity(n)
  class Quantity(val magnitude: Double) extends super.Quantity
}

object Length {
  object Inches extends Length(1, "Inches")
  object Feet extends Length(12, "Feet")
  object Yards extends Length(36, "Yard")
}

class Weight(val scale: Double, val name: String) extends AddableMeasure {
  type Q = Weight#Quantity
  def isComparableWith(that: Any) = that.isInstanceOf[Q]
  def apply(n: Double) = new Quantity(n)
  class Quantity(val magnitude: Double) extends super.Quantity
}

object Weight {
  object Gram extends Weight(1, "Gram")
  object Kilogram extends Weight(1000, "Kilogram")
  object Ton extends Weight(100000, "Ton")
}

abstract class Temperature(val name: String) extends Measure {
  def convertToBase(value: Double): Double

  type Q = Temperature#Quantity
  def isComparableWith(that: Any) = that.isInstanceOf[Q]
  def apply(n: Double) = new Quantity(n)

  class Quantity(val magnitude: Double) extends super.Quantity {
    def magnitudeInBaseUnit = convertToBase(magnitude)
  }
}

object Temperature {
  object Celcius extends Temperature("Celcius") {
    def convertToBase(value: Double) = value
  }

  object Fahrenheit extends Temperature("Fahrenheit") {
    def convertToBase(value: Double) = (value - 32) * 5 / 9
  }
}
