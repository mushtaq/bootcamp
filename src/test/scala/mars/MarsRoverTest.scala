package mars

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class MarsRoverTest extends Spec with ShouldMatchers{

  import Direction._

  def `rover change` {
    val rover = Rover(Point(3, 3), East)
    rover.change('L') should be(Rover(Point(3, 3), North))
    rover.change('R') should be(Rover(Point(3, 3), South))
    rover.change('M') should be(Rover(Point(4, 3), East))
  }

  def `rover move` {
    val rover = Rover(Point(3, 3), East)
    rover.finalState("LLLMRR") should be(Rover(Point(3, 2), North))
  }

  def `plateau die` {
    val rover = Rover(Point(3, 4), East)
    val plateau = Plateau(Point(0, 0), Point(4, 4), Set())

    val (plateauNew, roverNew) = plateau.navigate(rover, "MM")

    plateauNew should be(Plateau(Point(0, 0), Point(4, 4), Set(Rover(Point(5, 4), East))))
    roverNew should be(Rover(Point(5, 4), East))
    plateau.contains(roverNew) should be(false)
  }

  def `plateau survive` {
    val rover = Rover(Point(3, 4), East)
    val plateau = Plateau(Point(0, 0), Point(4, 4), Set(Rover(Point(5, 4), East)))

    val (plateauNew, roverNew) = plateau.navigate(rover, "MM")

    plateauNew should be(Plateau(Point(0, 0), Point(4, 4), Set(Rover(Point(5, 4), East))))
    roverNew should be(Rover(Point(4, 4), East))
    plateau.contains(roverNew) should be(true)
  }

}
