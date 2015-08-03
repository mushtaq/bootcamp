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
    def unit: Unit

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
