package parkinglot

import collection.mutable
import org.scala_tools.time.Imports._

class Car

class ParkingLot(lots: Int, owner: Owner = new Owner) extends mutable.Publisher[ParkingLotEvent] {

  type Pub = ParkingLot
  private val tokens: mutable.Buffer[Int] = (1 to lots).to[mutable.Buffer]
  private val parkings: mutable.Map[Int, Car] = mutable.Map()
  private var _lastParkingTime = DateTime.now

  def latestParkingTime = _lastParkingTime

  def currentStatus = ParkingLotStatusEvent(lots, parkings.size)

  def isParkingAvailable = tokens.nonEmpty
  def availableLots = tokens.size

  def unPark(token: Int): Option[Car] = {
    val carOption = parkings remove token
    if (carOption.isDefined) tokens += token
    publish(CarUnParkedEvent(lots, parkings.size, carOption, token))
    carOption
  }

  def park(car: Car): Option[Int] = {
    val tokenOption =
      if (tokens.isEmpty) None
      else {
        val token = tokens remove 0
        parkings += (token -> car)
        _lastParkingTime = DateTime.now
        Some(token)
      }
    publish(CarParkedEvent(lots, parkings.size, tokenOption, car))
    tokenOption
  }
}
