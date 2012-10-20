package parkinglot

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class ParkingLotSpec extends Spec with ShouldMatchers {

  object `car parking` {
    def `should be able to park if a token is available` {
      val parkingLot = new ParkingLot(1)

      val token = parkingLot.park(new Car)

      token.get should be(1)
    }

    def `should not be able to park if token not available` {
      val parkingLot = new ParkingLot(1)

      val token1 = parkingLot.park(new Car)
      val token2 = parkingLot.park(new Car)

      token1.get should be(1)
      token2.isEmpty should be(true)
    }
  }

  object `car unparking` {


    def `should be able to unpark a car if parked` {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar = parkingLot.unPark(token.get)

      unParkedCar.get should be(car)
    }

    def `should not be able to unpark a car twice` {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar1 = parkingLot.unPark(token.get)
      val unParkedCar2 = parkingLot.unPark(token.get)

      unParkedCar1.get should be(car)
      unParkedCar2.isEmpty should be(true)
    }

    def `should not be able to unpark a car if token is invalid` {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar1 = parkingLot.unPark(token.get + 100)

      unParkedCar1.isEmpty should be(true)
    }

    def `unparking should create space for a new car` {
      val parkingLot = new ParkingLot(1)

      val token1 = parkingLot.park(new Car)
      token1.get should be(1)

      val token2 = parkingLot.park(new Car)
      token2.isEmpty should be(true)

      parkingLot.unPark(token1.get)

      val token3 = parkingLot.park(new Car)
      token3.get should be(1)
    }

  }

  object `owner and agent notification` {

    def `owner should be notified when the lot has space again` {
      val owner = new Owner
      val parkingLot = new ParkingLot(2, owner)
      owner.subscribeTo(parkingLot, evt => evt.justCrossed(100))
      owner.subscribeTo(parkingLot, evt => evt.justCameBelow(100))

      owner.latestEventFor(parkingLot).get should be(ParkingLotStatusEvent(2, 0))

      parkingLot.park(new Car)
      owner.latestEventFor(parkingLot).get should be(ParkingLotStatusEvent(2, 0))

      val car = new Car
      val token = parkingLot.park(car)
      owner.latestEventFor(parkingLot).get should be(CarParkedEvent(2, 2, token, car))

      parkingLot.unPark(token.get)
      owner.latestEventFor(parkingLot).get should be(CarUnParkedEvent(2, 1, Some(car), token.get))

      parkingLot.unPark(token.get)
      owner.latestEventFor(parkingLot).get should be(CarUnParkedEvent(2, 1, Some(car), token.get))
    }

    def `agent should be notified when the garage is less than 80% full again` {
      val parkingLot = new ParkingLot(10)
      val fbiAgent = new FBIAgent
      fbiAgent.subscribeTo(parkingLot, evt => evt.justCrossed(80))
      fbiAgent.subscribeTo(parkingLot, evt => evt.justCameBelow(80))

      fbiAgent.latestEventFor(parkingLot).get should be(ParkingLotStatusEvent(10, 0))

      1 to 7 foreach (_ => parkingLot.park(new Car))
      fbiAgent.latestEventFor(parkingLot).get should be(ParkingLotStatusEvent(10, 0))

      val car = new Car
      val token = parkingLot.park(car)
      fbiAgent.latestEventFor(parkingLot).get should be(CarParkedEvent(10, 8, token, car))

      val token2 = parkingLot.park(new Car)
      val token3 = parkingLot.park(new Car)
      fbiAgent.latestEventFor(parkingLot).get should be(CarParkedEvent(10, 8, token, car))

      parkingLot.unPark(token.get)
      parkingLot.unPark(token2.get)
      fbiAgent.latestEventFor(parkingLot).get should be(CarParkedEvent(10, 8, token, car))

      val car2 = parkingLot.unPark(token3.get)
      fbiAgent.latestEventFor(parkingLot).get should be(CarUnParkedEvent(10, 7, car2, token3.get))
    }

    def `police department should be notified if a car is not found` {
      val parkingLot = new ParkingLot(0)
      val policeDept = new PoliceDept
      policeDept.subscribeTo(parkingLot, evt => evt.isCarMissing)

      policeDept.latestEventFor(parkingLot).get should be(ParkingLotStatusEvent(0, 0))

      parkingLot.unPark(1234)
      policeDept.latestEventFor(parkingLot).get should be(CarUnParkedEvent(0, 0, None, 1234))
    }

    def `FBI agent should be notified if a car is not found or lot is full` {
      val parkingLot = new ParkingLot(1)
      val fbiAgent = new FBIAgent
      fbiAgent.subscribeTo(parkingLot, evt => evt.isCarMissing)
      fbiAgent.subscribeTo(parkingLot, evt => evt.justCrossed(100))
      fbiAgent.subscribeTo(parkingLot, evt => evt.justCameBelow(100))

      fbiAgent.latestEventFor(parkingLot).get should be(ParkingLotStatusEvent(1, 0))
      parkingLot.unPark(1234)
      fbiAgent.latestEventFor(parkingLot).get should be(CarUnParkedEvent(1, 0, None, 1234))

      val car = new Car
      val token = parkingLot.park(car)
      fbiAgent.latestEventFor(parkingLot).get should be(CarParkedEvent(1, 1, token, car))
    }

    def `attendant parks car in a lot with space` {
      val parkingLot = new ParkingLot(1)
      val attendant = new ParkingLotAttendant
      attendant.subscribeTo(parkingLot, evt => evt.justCrossed(100))
      attendant.subscribeTo(parkingLot, evt => evt.justCameBelow(100))
      val car = new Car
      attendant.park(car).get should be(1)
      parkingLot.unPark(1).get should be(car)
    }

  }

}
