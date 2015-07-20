package parkinglot

import com.github.nscala_time.time.Imports._
import rx._

class Car

class ParkingLot(val lots: Int, owner: Owner = new Owner, val distance: Double = 0) {

  private val tokens = Var(Seq(1 to lots: _*))
  private val parkings = Var(Map.empty[Int, Car])

  val lastParkingTime = Var(DateTime.now)
  val status: Var[ParkingLotEvent] = Var(ParkingLotCreatedEvent(lots))

  def isParkingAvailable = tokens().nonEmpty
  def availableLots = tokens().size

  def unPark(token: Int) = {
    val carOption = parkings().get(token)
    if (carOption.isDefined) {
      tokens() :+= token
      parkings() -= token
    }
    status() = CarUnParkedEvent(lots, parkings().size, carOption, token)
    carOption
  }

  def park(car: Car) = {
    val tokenOption = tokens() match {
      case Seq()         =>
        None
      case token +: tail =>
        tokens() = tail
        parkings() += (token -> car)
        lastParkingTime() = DateTime.now
        Some(token)
    }
    status() = CarParkedEvent(lots, parkings().size, tokenOption, car)
    tokenOption
  }
}
