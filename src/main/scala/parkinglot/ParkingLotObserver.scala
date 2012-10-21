package parkinglot

import collection.mutable
import org.scala_tools.time.Imports._
import util.Try

trait ParkingLotObserver extends mutable.Subscriber[ParkingLotEvent, ParkingLot] {
  private val _parkingLotEvents: mutable.Map[ParkingLot, ParkingLotEvent] = mutable.Map()

  def lotsWithSpace = _parkingLotEvents.keys.collect{ case lot if lot.isParkingAvailable => lot}.to[Seq]

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
  def park(car: Car, lots: Option[ParkingLot]): Option[Int] = lots flatMap (parkingLot => parkingLot.park(car))

  def parkRandom(car: Car): Option[Int] = park(car, lotsWithSpace.headOption)
  def parkRoundRobin(car: Car): Option[Int] = park(car, Try(lotsWithSpace.minBy(_.latestParkingTime.millis)).toOption)
  def parkWithMaxSpace(car: Car): Option[Int] = park(car, Try(lotsWithSpace.maxBy(_.availableLots)).toOption)
}