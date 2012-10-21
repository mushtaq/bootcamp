package parkinglot

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class ParkingLotSpec extends Spec with ShouldMatchers {

  object `car parking` {
    def `should be able to park if a token is available` {
      val parkingLot = new ParkingLot(1)

      val token = parkingLot.park(new Car)

      token.get shouldBe 1
    }

    def `should not be able to park if token not available` {
      val parkingLot = new ParkingLot(1)

      val token1 = parkingLot.park(new Car)
      val token2 = parkingLot.park(new Car)

      token1.get shouldBe 1
      token2 shouldBe None
    }
  }

  object `car unparking` {


    def `should be able to unpark a car if parked` {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar = parkingLot.unPark(token.get)

      unParkedCar.get shouldBe car
    }

    def `should not be able to unpark a car twice` {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar1 = parkingLot.unPark(token.get)
      val unParkedCar2 = parkingLot.unPark(token.get)

      unParkedCar1.get shouldBe car
      unParkedCar2 shouldBe None
    }

    def `should not be able to unpark a car if token is invalid` {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar1 = parkingLot.unPark(token.get + 100)

      unParkedCar1 shouldBe None
    }

    def `unparking should create space for a new car` {
      val parkingLot = new ParkingLot(1)

      val token1 = parkingLot.park(new Car)
      token1.get shouldBe(1)

      val token2 = parkingLot.park(new Car)
      token2 shouldBe None

      parkingLot.unPark(token1.get)

      val token3 = parkingLot.park(new Car)
      token3.get shouldBe 1
    }

  }

  object `owner and agent notification` {

    def `owner should be notified when the lot has space again` {
      val owner = new Owner
      val parkingLot = new ParkingLot(2, owner)
      owner.subscribeTo(parkingLot, evt => evt.justCrossed(100))

      owner.latestEventFor(parkingLot).get shouldBe ParkingLotStatusEvent(2, 0)

      parkingLot.park(new Car)
      owner.latestEventFor(parkingLot).get shouldBe ParkingLotStatusEvent(2, 0)

      val car = new Car
      val token = parkingLot.park(car)
      owner.latestEventFor(parkingLot).get shouldBe CarParkedEvent(2, 2, token, car)

      parkingLot.unPark(token.get)
      owner.latestEventFor(parkingLot).get shouldBe CarUnParkedEvent(2, 1, Some(car), token.get)

      parkingLot.unPark(token.get)
      owner.latestEventFor(parkingLot).get shouldBe CarUnParkedEvent(2, 1, Some(car), token.get)
    }

    def `agent should be notified when the garage is less than 80% full again` {
      val parkingLot = new ParkingLot(10)
      val fbiAgent = new FBIAgent
      fbiAgent.subscribeTo(parkingLot, evt => evt.justCrossed(80))

      fbiAgent.latestEventFor(parkingLot).get shouldBe ParkingLotStatusEvent(10, 0)

      1 to 7 foreach (_ => parkingLot.park(new Car))
      fbiAgent.latestEventFor(parkingLot).get shouldBe ParkingLotStatusEvent(10, 0)

      val car = new Car
      val token = parkingLot.park(car)
      fbiAgent.latestEventFor(parkingLot).get shouldBe CarParkedEvent(10, 8, token, car)

      val token2 = parkingLot.park(new Car)
      val token3 = parkingLot.park(new Car)
      fbiAgent.latestEventFor(parkingLot).get shouldBe CarParkedEvent(10, 8, token, car)

      parkingLot.unPark(token.get)
      parkingLot.unPark(token2.get)
      fbiAgent.latestEventFor(parkingLot).get shouldBe CarParkedEvent(10, 8, token, car)

      val car2 = parkingLot.unPark(token3.get)
      fbiAgent.latestEventFor(parkingLot).get shouldBe CarUnParkedEvent(10, 7, car2, token3.get)
    }

    def `police department should be notified if a car is not found` {
      val parkingLot = new ParkingLot(0)
      val policeDept = new PoliceDept
      policeDept.subscribeTo(parkingLot, evt => evt.isCarMissing)

      policeDept.latestEventFor(parkingLot).get shouldBe ParkingLotStatusEvent(0, 0)

      parkingLot.unPark(1234)
      policeDept.latestEventFor(parkingLot).get shouldBe CarUnParkedEvent(0, 0, None, 1234)
    }

    def `FBI agent should be notified if a car is not found or lot is full` {
      val parkingLot = new ParkingLot(1)
      val fbiAgent = new FBIAgent
      fbiAgent.subscribeTo(parkingLot, evt => evt.isCarMissing)
      fbiAgent.subscribeTo(parkingLot, evt => evt.justCrossed(100))

      fbiAgent.latestEventFor(parkingLot).get shouldBe ParkingLotStatusEvent(1, 0)
      parkingLot.unPark(1234)
      fbiAgent.latestEventFor(parkingLot).get shouldBe CarUnParkedEvent(1, 0, None, 1234)

      val car = new Car
      val token = parkingLot.park(car)
      fbiAgent.latestEventFor(parkingLot).get shouldBe CarParkedEvent(1, 1, token, car)
    }

    def `attendant parks car in a lot with space` {
      val parkingLot = new ParkingLot(1)
      val parkingLot2 = new ParkingLot(0)
      val attendant = new ParkingLotAttendant

      attendant.subscribeTo(parkingLot, evt => evt.justCrossed(100))
      attendant.subscribeTo(parkingLot2, evt => evt.justCrossed(100))

      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(1, 0)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(0, 0)

      attendant.parkRandom(new Car).get shouldBe 1

      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(1, 1)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(0, 0)

    }

    def `attendant parks car evenly in a lot with space` {
      val parkingLot = new ParkingLot(2)
      Thread.sleep(1)
      val parkingLot2 = new ParkingLot(2)
      val attendant = new ParkingLotAttendant

      attendant.subscribeToAllEvents(parkingLot)
      attendant.subscribeToAllEvents(parkingLot2)

      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(2, 0)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(2, 0)

      attendant.parkRoundRobin(new Car).get shouldBe 1
      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(2, 1)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(2, 0)

      attendant.parkRoundRobin(new Car).get shouldBe 1
      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(2, 1)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(2, 1)
    }

    def `attendant parks car in a lot with max space` {
      val parkingLot = new ParkingLot(4)
      val parkingLot2 = new ParkingLot(2)
      val attendant = new ParkingLotAttendant

      attendant.subscribeToAllEvents(parkingLot)
      attendant.subscribeToAllEvents(parkingLot2)

      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(4, 0)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(2, 0)

      attendant.parkWithMaxSpace(new Car).get shouldBe 1
      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(4, 1)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(2, 0)
    }

    def `attendant parks car in a lot which is closest` {
      val parkingLot = new ParkingLot(2, distance = 10)
      val parkingLot2 = new ParkingLot(2, distance = 20)
      val attendant = new ParkingLotAttendant

      attendant.subscribeToAllEvents(parkingLot)
      attendant.subscribeToAllEvents(parkingLot2)

      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(2, 0)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(2, 0)

      attendant.parkWithMaxSpace(new Car).get shouldBe 1
      parkingLot.currentStatus shouldBe ParkingLotStatusEvent(2, 1)
      parkingLot2.currentStatus shouldBe ParkingLotStatusEvent(2, 0)
    }

  }

}
