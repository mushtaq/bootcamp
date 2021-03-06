package unit.members

trait ScaledMeasure extends Measure {
  type Q = ScaledQuantity

  trait ScaledUnit extends Unit {
    def scale: Double

    def convertToBaseUnit(magnitude: Double) = magnitude * scale
    def convertFromBaseUnit(magnitude: Double) = magnitude / scale
  }

  class ScaledQuantity(val magnitude: Double, val unit: U) extends Quantity {
    def +(that: ScaledQuantity): ScaledQuantity =
      unit(unit.convertFromBaseUnit(magnitudeInBaseUnit + that.magnitudeInBaseUnit))
  }
}
