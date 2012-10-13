package parkinglot

import collection.mutable

class Car

class ParkingLot(lots: Int, owner: Owner = new Owner) extends mutable.Publisher[ParkingLotEvent] {

  type Pub = ParkingLot
  private val tokens: mutable.Buffer[Int] = (1 to lots).to[mutable.Buffer]
  private val parkings: mutable.Map[Int, Car] = mutable.Map()

  def unPark(token: Int): Option[Car] = {
    val carOption = parkings remove token
    if (carOption.isDefined) tokens += token
    publish(CarUnParked(lots, parkings.size, carOption, token))
    carOption
  }

  def park(car: Car): Option[Int] = {
    val tokenOption =
      if (tokens.isEmpty) None
      else {
        val token = tokens remove 0
        parkings += (token -> car)
        Some(token)
      }
    publish(CarParked(lots, parkings.size, tokenOption, car))
    tokenOption
  }
}
