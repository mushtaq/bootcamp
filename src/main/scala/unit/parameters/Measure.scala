package unit.parameters

trait Unit[U <: Unit[U, Q], Q <: Quantity[U, Q]] {
  def name: String
  def apply(magnitude: Double): Q
  def convertToBaseUnit(magnitude: Double): Double
  def convertFromBaseUnit(magnitude: Double): Double
  def canBeConvertedTo(that: Unit[_, _]): Boolean
}

trait Quantity[U <: Unit[U, Q], Q <: Quantity[U, Q]] {
  def magnitude: Double
  def unit: U

  def magnitudeInBaseUnit = unit.convertToBaseUnit(magnitude)
  def in(thatUnit: U) = thatUnit(thatUnit.convertFromBaseUnit(magnitudeInBaseUnit))

  override def equals(that: Any) = that match {
    case x: Quantity[_, _] => unit.canBeConvertedTo(x.unit) && this.magnitudeInBaseUnit == x.magnitudeInBaseUnit
    case _                 => false
  }

  override def toString = s"$magnitude ${unit.name}"
}
