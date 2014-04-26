package parkinglot

import collection.mutable
import com.github.nscala_time.time.Imports._
import util.Try

trait ParkingLotObserver extends mutable.Subscriber[ParkingLotEvent, ParkingLot] {
  private val _parkingLotEvents: mutable.Map[ParkingLot, ParkingLotEvent] = mutable.Map()

  def lotsWithSpace = _parkingLotEvents.keys.filter(_.isParkingAvailable).to[Seq]

  def latestEventFor(parkingLot: ParkingLot) = _parkingLotEvents.get(parkingLot)

  def subscribeTo(parkingLot: ParkingLot, filter: ParkingLotEvent => Boolean) {
    _parkingLotEvents(parkingLot) = parkingLot.currentStatus
    parkingLot.subscribe(this, filter)
  }

  def subscribeToAllEvents(parkingLot: ParkingLot) {
    _parkingLotEvents(parkingLot) = parkingLot.currentStatus
    parkingLot.subscribe(this)
  }

  def notify(pub: ParkingLot, event: ParkingLotEvent) {
    _parkingLotEvents(pub) = event
  }
}

class Owner extends ParkingLotObserver
class FBIAgent extends ParkingLotObserver
class PoliceDept extends ParkingLotObserver

class ParkingLotAttendant extends ParkingLotObserver {
  def park(car: Car, lots: Try[ParkingLot]): Option[Int] = lots.toOption flatMap (parkingLot => parkingLot.park(car))

  def parkRandom(car: Car): Option[Int] = park(car, Try(lotsWithSpace.head))
  def parkRoundRobin(car: Car): Option[Int] = park(car, Try(lotsWithSpace.minBy(_.latestParkingTime.millis)))
  def parkWithMaxSpace(car: Car): Option[Int] = park(car, Try(lotsWithSpace.maxBy(_.availableLots)))
  def parkInClosest(car: Car): Option[Int] = park(car, Try(lotsWithSpace.minBy(_.distance)))
}