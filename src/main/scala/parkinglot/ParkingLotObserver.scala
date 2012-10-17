package parkinglot

import collection.mutable

trait ParkingLotObserver extends mutable.Subscriber[ParkingLotEvent, ParkingLot] {
  private var _event: ParkingLotEvent = _
  def event = _event
  def notify(pub: ParkingLot, event: ParkingLotEvent) {
    _event = event
  }
}

class Owner extends ParkingLotObserver
class FBIAgent extends ParkingLotObserver
class PoliceDept extends ParkingLotObserver
