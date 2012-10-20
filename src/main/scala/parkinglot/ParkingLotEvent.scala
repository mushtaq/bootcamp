package parkinglot

sealed trait ParkingLotEvent {
  def totalLots: Int
  def occupiedLots: Int

  def availableLots = totalLots - occupiedLots

  def justCrossed(threshold: Double) = false
  def justCameBelow(threshold: Double) = false
  def isCarMissing = false

  protected def preEventOccupiedLots: Int = occupiedLots
  protected def occupiedPercent = percentOf(occupiedLots)
  protected def preEventOccupiedPercent = percentOf(preEventOccupiedLots)

  private def percentOf(lots: Int) = 100.0 * lots / totalLots
}

case class ParkingLotStatusEvent(totalLots: Int, occupiedLots: Int) extends ParkingLotEvent

case class CarParkedEvent(totalLots: Int, occupiedLots: Int, token: Option[Int], car: Car) extends ParkingLotEvent {
  override def justCrossed(threshold: Double) = preEventOccupiedPercent < threshold && occupiedPercent >= threshold
  override def preEventOccupiedLots = occupiedLots - token.size
}

case class CarUnParkedEvent(totalLots: Int, occupiedLots: Int, car: Option[Car], token: Int) extends ParkingLotEvent {
  override def justCameBelow(threshold: Double) = occupiedPercent < threshold && preEventOccupiedPercent >= threshold
  override def isCarMissing = car.isEmpty
  override def preEventOccupiedLots = occupiedLots + car.size
}
