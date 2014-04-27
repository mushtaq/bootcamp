package parkinglot

sealed trait ParkingLotEvent {
  def totalLots: Int
  def occupiedLots: Int

  def preEventOccupiedLots: Int
  def isCarMissing: Boolean
  def justWentAbove(threshold: Double): Boolean
  def justCameBelow(threshold: Double): Boolean

  def justCrossed(threshold: Double) = justWentAbove(threshold) || justCameBelow(threshold)
  def occupiedPercent = percentOf(occupiedLots)
  def preEventOccupiedPercent = percentOf(preEventOccupiedLots)

  def percentOf(lots: Int) = 100.0 * lots / totalLots
}

case class ParkingLotCreatedEvent(totalLots: Int, occupiedLots: Int) extends ParkingLotEvent {
  def preEventOccupiedLots = occupiedLots
  def isCarMissing = false
  def justWentAbove(threshold: Double) = false
  def justCameBelow(threshold: Double) = false
}

case class CarParkedEvent(totalLots: Int, occupiedLots: Int, token: Option[Int], car: Car) extends ParkingLotEvent {
  def preEventOccupiedLots = occupiedLots - token.size
  def isCarMissing = false
  def justWentAbove(threshold: Double) = preEventOccupiedPercent < threshold && threshold <= occupiedPercent
  def justCameBelow(threshold: Double) = false
}

case class CarUnParkedEvent(totalLots: Int, occupiedLots: Int, car: Option[Car], token: Int) extends ParkingLotEvent {
  def preEventOccupiedLots = occupiedLots + car.size
  def isCarMissing = car.isEmpty
  def justWentAbove(threshold: Double) = false
  def justCameBelow(threshold: Double) = occupiedPercent < threshold && threshold <= preEventOccupiedPercent
}
