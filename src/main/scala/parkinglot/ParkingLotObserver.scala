package parkinglot

import collection.mutable

trait ParkingLotObserver extends mutable.Subscriber[ParkingLotEvent, ParkingLot] {
  private val _parkingLotEvents: mutable.Map[ParkingLot, ParkingLotEvent] = mutable.Map()

  def lotsWithSpace = _parkingLotEvents.filter(_._2.availableLots > 0).map(_._1)

  def latestEventFor(parkingLot: ParkingLot) = _parkingLotEvents.get(parkingLot)

  def subscribeTo(parkingLot: ParkingLot, filter: ParkingLotEvent => Boolean) {
    _parkingLotEvents(parkingLot) = parkingLot.currentStatus
    parkingLot.subscribe(this, filter)
  }

  def notify(pub: ParkingLot, event: ParkingLotEvent) {
    _parkingLotEvents(pub) = event
  }
}

class Owner extends ParkingLotObserver
class FBIAgent extends ParkingLotObserver
class PoliceDept extends ParkingLotObserver

class ParkingLotAttendant extends ParkingLotObserver {
  def park(car: Car): Option[Int] = lotsWithSpace.headOption flatMap (parkingLot => parkingLot.park(car))
}