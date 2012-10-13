package parkinglot

import collection.mutable

trait ParkingLotObserver extends mutable.Subscriber[ParkingLotEvent, ParkingLot] {
  private var _event: ParkingLotEvent = _
  def event = _event
  def notify(pub: ParkingLot, event: ParkingLotEvent) {
    _event = event
  }

  def subscribeForOccupancy(parkingLot: ParkingLot, withThreshold: Double) {
    parkingLot.subscribe(this, _.filter(withThreshold))
  }

  def subscribeForMissingCar(parkingLot: ParkingLot) {
    parkingLot.subscribe(this, isCarMissing)
  }

  private def isCarMissing(event: ParkingLotEvent) = event match {
    case CarUnParked(_, _, None, _) => true
    case _ => false
  }

}

class Owner extends ParkingLotObserver
class FBIAgent extends ParkingLotObserver
class PoliceDept extends ParkingLotObserver
