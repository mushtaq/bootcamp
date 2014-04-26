package mars

import org.specs2.mutable.Specification

class MarsRoverTest extends Specification {

  import Direction._

  "rover change" in  {
    val rover = Rover(Point(3, 3), East)
    rover.change('L') mustEqual Rover(Point(3, 3), North)
    rover.change('R') mustEqual Rover(Point(3, 3), South)
    rover.change('M') mustEqual Rover(Point(4, 3), East)
  }

  "rover move" in  {
    val rover = Rover(Point(3, 3), East)
    rover.finalState("LLLMRR") mustEqual Rover(Point(3, 2), North)
  }

  "plateau die" in  {
    val rover = Rover(Point(3, 4), East)
    val plateau = Plateau(Point(0, 0), Point(4, 4), Set())

    val (plateauNew, roverNew) = plateau.navigate(rover, "MM")

    plateauNew mustEqual Plateau(Point(0, 0), Point(4, 4), Set(Rover(Point(5, 4), East)))
    roverNew mustEqual Rover(Point(5, 4), East)
    plateau.contains(roverNew) must beFalse
  }

  "plateau survive" in  {
    val rover = Rover(Point(3, 4), East)
    val plateau = Plateau(Point(0, 0), Point(4, 4), Set(Rover(Point(5, 4), East)))

    val (plateauNew, roverNew) = plateau.navigate(rover, "MM")

    plateauNew mustEqual Plateau(Point(0, 0), Point(4, 4), Set(Rover(Point(5, 4), East)))
    roverNew mustEqual Rover(Point(4, 4), East)
    plateau.contains(roverNew) must beTrue
  }

}
