package unit.members2

trait ScaledMeasure extends Measure {
  type Q = Quantity

  trait Unit extends super.Unit {
    def scale: Double

    def apply(magnitude: Double) = new Quantity(magnitude, this)

    def convertToBaseUnit(magnitude: Double) = magnitude * scale
    def convertFromBaseUnit(magnitude: Double) = magnitude / scale
  }

  class Quantity(val magnitude: Double, val unit: Unit) extends super.Quantity {
    def +(that: Q): Q = unit(unit.convertFromBaseUnit(this.magnitudeInBaseUnit + that.magnitudeInBaseUnit))
  }
}
