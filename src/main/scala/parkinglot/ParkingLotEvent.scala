package parkinglot

sealed trait ParkingLotEvent {
  def totalLots: Int
  def occupiedSlots: Int
  def filter(threshold: Double): Boolean

  protected def percentOf(lots: Int) = 100.0 * lots / totalLots
  protected def postEventPercent = percentOf(occupiedSlots)
}

case class CarParked(totalLots: Int, occupiedSlots: Int, token: Option[Int], car: Car) extends ParkingLotEvent {
  def preEventPercent = percentOf(occupiedSlots - token.size)
  def filter(threshold: Double) = preEventPercent < threshold && postEventPercent >= threshold
}

case class CarUnParked(totalLots: Int, occupiedSlots: Int, car: Option[Car], token: Int) extends ParkingLotEvent {
  def preEventPercent = percentOf(occupiedSlots + car.size)
  def filter(threshold: Double) = postEventPercent < threshold && preEventPercent >= threshold
}
