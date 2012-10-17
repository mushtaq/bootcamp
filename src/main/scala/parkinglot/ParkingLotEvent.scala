package parkinglot

sealed trait ParkingLotEvent {
  def justCrossed(threshold: Double) = false
  def justCameBelow(threshold: Double) = false
  def isCarMissing = false

  def totalLots: Int
  def occupiedLots: Int
  protected def preEventOccupiedSlots: Int

  protected def occupiedPercent = percentOf(occupiedLots)
  protected def preEventOccupiedPercent = percentOf(preEventOccupiedSlots)

  private def percentOf(lots: Int) = 100.0 * lots / totalLots
}

case class CarParked(totalLots: Int, occupiedLots: Int, token: Option[Int], car: Car) extends ParkingLotEvent {
  override def justCrossed(threshold: Double) = preEventOccupiedPercent < threshold && occupiedPercent >= threshold
  def preEventOccupiedSlots = occupiedLots - token.size
}

case class CarUnParked(totalLots: Int, occupiedLots: Int, car: Option[Car], token: Int) extends ParkingLotEvent {
  override def justCameBelow(threshold: Double) = occupiedPercent < threshold && preEventOccupiedPercent >= threshold
  override def isCarMissing = car.isEmpty
  def preEventOccupiedSlots = occupiedLots + car.size
}
