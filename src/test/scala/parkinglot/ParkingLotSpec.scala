package parkinglot

import org.specs2.mutable.Specification

class ParkingLotSpec extends Specification {

  val car1 = new Car
  val car2 = new Car

  "car parking" should {
    "should be able to park if a token is available" in {
      val parkingLot = new ParkingLot(1)

      val token = parkingLot.park(new Car)

      token.get mustEqual 1
    }

    "should not be able to park if token not available" in {
      val parkingLot = new ParkingLot(1)

      val token1 = parkingLot.park(new Car)
      val token2 = parkingLot.park(new Car)

      token1.get mustEqual 1
      token2 mustEqual None
    }
  }

  "car unparking" should {


    "should be able to unpark a car if parked" in {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar = parkingLot.unPark(token.get)

      unParkedCar.get mustEqual car
    }

    "should not be able to unpark a car twice" in {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar1 = parkingLot.unPark(token.get)
      val unParkedCar2 = parkingLot.unPark(token.get)

      unParkedCar1.get mustEqual car
      unParkedCar2 mustEqual None
    }

    "should not be able to unpark a car if token is invalid" in {
      val parkingLot = new ParkingLot(1)
      val car = new Car

      val token = parkingLot.park(car)
      val unParkedCar1 = parkingLot.unPark(token.get + 100)

      unParkedCar1 mustEqual None
    }

    "unparking should create space for a new car" in {
      val parkingLot = new ParkingLot(1)

      val token1 = parkingLot.park(new Car)
      token1.get mustEqual(1)

      val token2 = parkingLot.park(new Car)
      token2 mustEqual None

      parkingLot.unPark(token1.get)

      val token3 = parkingLot.park(new Car)
      token3.get mustEqual 1
    }

  }

  "owner and agent notification" should {

    "owner should be notified when the lot has space again" in {
      val owner = new Owner
      val parkingLot = new ParkingLot(2, owner)
      owner.subscribeTo(parkingLot, evt => evt.justCrossed(100))

      owner.latestEventFor(parkingLot).get mustEqual ParkingLotCreatedEvent(2, 0)

      parkingLot.park(new Car)
      owner.latestEventFor(parkingLot).get mustEqual ParkingLotCreatedEvent(2, 0)

      val car = new Car
      val token = parkingLot.park(car)
      owner.latestEventFor(parkingLot).get mustEqual CarParkedEvent(2, 2, token, car)

      parkingLot.unPark(token.get)
      owner.latestEventFor(parkingLot).get mustEqual CarUnParkedEvent(2, 1, Some(car), token.get)

      parkingLot.unPark(token.get)
      owner.latestEventFor(parkingLot).get mustEqual CarUnParkedEvent(2, 1, Some(car), token.get)
    }

    "agent should be notified when the garage is less than 80% full again" in {
      val parkingLot = new ParkingLot(10)
      val fbiAgent = new FBIAgent
      fbiAgent.subscribeTo(parkingLot, evt => evt.justCrossed(80))

      fbiAgent.latestEventFor(parkingLot).get mustEqual ParkingLotCreatedEvent(10, 0)

      1 to 7 foreach (_ => parkingLot.park(new Car))
      fbiAgent.latestEventFor(parkingLot).get mustEqual ParkingLotCreatedEvent(10, 0)

      val car = new Car
      val token = parkingLot.park(car)
      fbiAgent.latestEventFor(parkingLot).get mustEqual CarParkedEvent(10, 8, token, car)

      val token2 = parkingLot.park(new Car)
      val token3 = parkingLot.park(new Car)
      fbiAgent.latestEventFor(parkingLot).get mustEqual CarParkedEvent(10, 8, token, car)

      parkingLot.unPark(token.get)
      parkingLot.unPark(token2.get)
      fbiAgent.latestEventFor(parkingLot).get mustEqual CarParkedEvent(10, 8, token, car)

      val car2 = parkingLot.unPark(token3.get)
      fbiAgent.latestEventFor(parkingLot).get mustEqual CarUnParkedEvent(10, 7, car2, token3.get)
    }

    "police department should be notified if a car is not found" in {
      val parkingLot = new ParkingLot(0)
      val policeDept = new PoliceDept
      policeDept.subscribeTo(parkingLot, evt => evt.isCarMissing)

      policeDept.latestEventFor(parkingLot).get mustEqual ParkingLotCreatedEvent(0, 0)

      parkingLot.unPark(1234)
      policeDept.latestEventFor(parkingLot).get mustEqual CarUnParkedEvent(0, 0, None, 1234)
    }

    "FBI agent should be notified if a car is not found or lot is full" in {
      val parkingLot = new ParkingLot(1)
      val fbiAgent = new FBIAgent
      fbiAgent.subscribeTo(parkingLot, evt => evt.isCarMissing)
      fbiAgent.subscribeTo(parkingLot, evt => evt.justCrossed(100))

      fbiAgent.latestEventFor(parkingLot).get mustEqual ParkingLotCreatedEvent(1, 0)
      parkingLot.unPark(1234)
      fbiAgent.latestEventFor(parkingLot).get mustEqual CarUnParkedEvent(1, 0, None, 1234)

      val car = new Car
      val token = parkingLot.park(car)
      fbiAgent.latestEventFor(parkingLot).get mustEqual CarParkedEvent(1, 1, token, car)
    }

    "attendant parks car in a lot with space" in {
      val parkingLot = new ParkingLot(1)
      val parkingLot2 = new ParkingLot(0)
      val attendant = new ParkingLotAttendant

      attendant.subscribeTo(parkingLot, evt => evt.justCrossed(100))
      attendant.subscribeTo(parkingLot2, evt => evt.justCrossed(100))

      parkingLot.status() mustEqual ParkingLotCreatedEvent(1, 0)
      parkingLot2.status() mustEqual ParkingLotCreatedEvent(0, 0)

      attendant.parkRandom(car1).get mustEqual 1

      parkingLot.status() mustEqual CarParkedEvent(1,1,Some(1),car1)
      parkingLot2.status() mustEqual ParkingLotCreatedEvent(0, 0)

    }

    "attendant parks car evenly in a lot with space" in {
      val parkingLot = new ParkingLot(2)
      Thread.sleep(1)
      val parkingLot2 = new ParkingLot(2)
      val attendant = new ParkingLotAttendant

      attendant.subscribeToAllEvents(parkingLot)
      attendant.subscribeToAllEvents(parkingLot2)

      parkingLot.status() mustEqual ParkingLotCreatedEvent(2, 0)
      parkingLot2.status() mustEqual ParkingLotCreatedEvent(2, 0)

      attendant.parkRoundRobin(car1).get mustEqual 1
      parkingLot.status() mustEqual CarParkedEvent(2,1,Some(1),car1)
      parkingLot2.status() mustEqual ParkingLotCreatedEvent(2, 0)

      attendant.parkRoundRobin(car2).get mustEqual 1
      parkingLot.status() mustEqual CarParkedEvent(2,1,Some(1),car1)
      parkingLot2.status() mustEqual CarParkedEvent(2,1,Some(1),car2)
    }

    "attendant parks car in a lot with max space" in {
      val parkingLot = new ParkingLot(4)
      val parkingLot2 = new ParkingLot(2)
      val attendant = new ParkingLotAttendant

      attendant.subscribeToAllEvents(parkingLot)
      attendant.subscribeToAllEvents(parkingLot2)

      parkingLot.status() mustEqual ParkingLotCreatedEvent(4, 0)
      parkingLot2.status() mustEqual ParkingLotCreatedEvent(2, 0)

      attendant.parkWithMaxSpace(car1).get mustEqual 1
      parkingLot.status() mustEqual CarParkedEvent(4,1,Some(1),car1)
      parkingLot2.status() mustEqual ParkingLotCreatedEvent(2, 0)
    }

    "attendant parks car in a lot which is closest" in {
      val parkingLot = new ParkingLot(2, distance = 10)
      val parkingLot2 = new ParkingLot(2, distance = 20)
      val attendant = new ParkingLotAttendant

      attendant.subscribeToAllEvents(parkingLot)
      attendant.subscribeToAllEvents(parkingLot2)

      parkingLot.status() mustEqual ParkingLotCreatedEvent(2, 0)
      parkingLot2.status() mustEqual ParkingLotCreatedEvent(2, 0)

      attendant.parkInClosest(car1).get mustEqual 1
      parkingLot.status() mustEqual CarParkedEvent(2,1,Some(1),car1)
      parkingLot2.status() mustEqual ParkingLotCreatedEvent(2, 0)
    }

  }

}
