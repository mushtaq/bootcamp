package parkinglot

import com.github.nscala_time.time.Imports._
import util.Try
import rx._
import rx.ops.RxOps

trait ParkingLotObserver {
  private val parkingLotEvents = Var(Map.empty[ParkingLot, ParkingLotEvent])

  def lotsWithSpace = parkingLotEvents().keys.filter(_.isParkingAvailable).to[Seq]

  def latestEventFor(parkingLot: ParkingLot) = parkingLotEvents().get(parkingLot)

  def subscribeTo(parkingLot: ParkingLot, filter: ParkingLotEvent => Boolean) = {
    val event = parkingLot.status.filter(filter)
    Obs(event) {
      parkingLotEvents() += parkingLot -> event()
    }
  }

  def subscribeToAllEvents(parkingLot: ParkingLot) = subscribeTo(parkingLot, evt => true)
}

class Owner extends ParkingLotObserver
class FBIAgent extends ParkingLotObserver
class PoliceDept extends ParkingLotObserver

class ParkingLotAttendant extends ParkingLotObserver {
  def park(car: Car, lots: Try[ParkingLot]): Option[Int] = lots.toOption flatMap (parkingLot => parkingLot.park(car))

  def parkRandom(car: Car): Option[Int] = park(car, Try(lotsWithSpace.head))
  def parkRoundRobin(car: Car): Option[Int] = park(car, Try(lotsWithSpace.minBy(_.lastParkingTime().millis)))
  def parkWithMaxSpace(car: Car): Option[Int] = park(car, Try(lotsWithSpace.maxBy(_.availableLots)))
  def parkInClosest(car: Car): Option[Int] = park(car, Try(lotsWithSpace.minBy(_.distance)))
}
