package parkinglot

import collection.mutable

trait ParkingLotObserver extends mutable.Subscriber[ParkingLotEvent, ParkingLot] {
  private var _event: ParkingLotEvent = _
  def event = _event
  def notify(pub: ParkingLot, event: ParkingLotEvent) {
    _event = event
  }

  def subscribeTo(parkingLot: ParkingLot, withThreshold: Double) {
    parkingLot.subscribe(this, _.filter(withThreshold))
  }

}

class Owner extends ParkingLotObserver
class FBIAgent extends ParkingLotObserver
