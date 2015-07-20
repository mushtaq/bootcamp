package parkinglot

sealed trait ParkingLotEvent {
  def totalLots: Int
  def occupiedLots: Int
  def preEventOccupiedLots: Int

  def justCrossed(threshold: Double) = {
    val min = occupiedPercent.min(preEventOccupiedPercent)
    val max = occupiedPercent.max(preEventOccupiedPercent)
    min < threshold && threshold <= max
  }

  def occupiedPercent = percentOf(occupiedLots)
  def preEventOccupiedPercent = percentOf(preEventOccupiedLots)
  def percentOf(lots: Int) = 100.0 * lots / totalLots
}

case class ParkingLotCreatedEvent(totalLots: Int) extends ParkingLotEvent {
  def occupiedLots = 0
  def preEventOccupiedLots = 0
}

case class CarParkedEvent(totalLots: Int, occupiedLots: Int, token: Option[Int], car: Car) extends ParkingLotEvent {
  def preEventOccupiedLots = occupiedLots - token.size
}

case class CarUnParkedEvent(totalLots: Int, occupiedLots: Int, car: Option[Car], token: Int) extends ParkingLotEvent {
  def preEventOccupiedLots = occupiedLots + car.size
}
