package unit.members2

trait Measure {
  type U <: Unit
  type Q <: Quantity

  trait Unit {
    def name: String
    def apply(magnitude: Double): Q
    def convertToBaseUnit(magnitude: Double): Double
    def convertFromBaseUnit(magnitude: Double): Double
  }

  trait Quantity {
    def magnitude: Double
    def unit: U

    def magnitudeInBaseUnit = unit.convertToBaseUnit(magnitude)
    def in(thatUnit: U) = thatUnit(thatUnit.convertFromBaseUnit(magnitudeInBaseUnit))

    override def equals(that: Any) = that match {
      case x: Quantity => this.magnitudeInBaseUnit == x.magnitudeInBaseUnit
      case _           => false
    }

    override def toString = s"$magnitude ${unit.name}"
  }
}
