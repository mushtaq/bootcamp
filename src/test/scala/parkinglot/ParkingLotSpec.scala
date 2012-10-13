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
      owner.subscribeTo(parkingLot, 100)

      owner.event should be(null)

      parkingLot.park(new Car)
      owner.event should be(null)

      val token = parkingLot.park(new Car)
      owner.event should be(CarParked(2, 2, token))

      val car = parkingLot.unPark(token.get)
      owner.event should be(CarUnParked(2, 1, car))

      parkingLot.unPark(token.get)
      owner.event should be(CarUnParked(2, 1, car))
    }

    def `agent should be notified when the garage is less than 80% full again` {
      val parkingLot = new ParkingLot(10)
      val fbiAgent = new FBIAgent
      fbiAgent.subscribeTo(parkingLot, 80)

      fbiAgent.event should be(null)

      1 to 7 foreach (_ => parkingLot.park(new Car))
      fbiAgent.event should be(null)

      val token = parkingLot.park(new Car)
      fbiAgent.event should be(CarParked(10, 8, token))

      val token2 = parkingLot.park(new Car)
      val token3 = parkingLot.park(new Car)
      fbiAgent.event should be(CarParked(10, 8, token))

      parkingLot.unPark(token.get)
      parkingLot.unPark(token2.get)
      fbiAgent.event should be(CarParked(10, 8, token))

      val car = parkingLot.unPark(token3.get)
      fbiAgent.event should be(CarUnParked(10, 7, car))
    }

  }

}
