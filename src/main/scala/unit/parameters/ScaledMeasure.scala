package unit.parameters

trait ScaledUnit[U <: Unit[U, Q], Q <: Quantity[U, Q]] extends Unit[U, Q] {
  def scale: Double

  def convertToBaseUnit(magnitude: Double) = magnitude * scale
  def convertFromBaseUnit(magnitude: Double) = magnitude / scale
}

class ScaledQuantity[U <: Unit[U, ScaledQuantity[U]]](val magnitude: Double, val unit: U) extends Quantity[U, ScaledQuantity[U]] {
  def +(that: ScaledQuantity[U]): ScaledQuantity[U] = unit(unit.convertFromBaseUnit(magnitudeInBaseUnit + that.magnitudeInBaseUnit))
}
