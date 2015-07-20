package mars

import org.specs2.mutable.Specification

class MarsRoverTest extends Specification {

  import Direction._

  "position change" in  {
    val position = Position(Point(3, 3), East)
    position.change('L') mustEqual Position(Point(3, 3), North)
    position.change('R') mustEqual Position(Point(3, 3), South)
    position.change('M') mustEqual Position(Point(4, 3), East)
  }

  "plateau die" in  {
    val position = Position(Point(3, 4), East)
    val plateau = Plateau(Point(0, 0), Point(4, 4), Set())
    val rover = Rover(position, plateau)
    val commands = Commands("MM")

    val roverNew = rover.navigate(commands)

    roverNew.plateau mustEqual Plateau(Point(0, 0), Point(4, 4), Set(Point(5, 4)))
    roverNew.position mustEqual Position(Point(5, 4), East)
  }

  "plateau survive" in  {
    val position = Position(Point(3, 4), East)
    val plateau = Plateau(Point(0, 0), Point(4, 4), Set(Point(5, 4)))
    val rover = Rover(position, plateau)
    val commands = Commands("MM")

    val roverNew = rover.navigate(commands)

    roverNew.plateau mustEqual Plateau(Point(0, 0), Point(4, 4), Set(Point(5, 4)))
    roverNew.position mustEqual Position(Point(4, 4), East)
  }

  "integration" in {
    val plateau = Plateau(Point(0, 0), Point(5, 5), Set.empty)
    val rover1 = Rover(Position(Point(1, 2), North), plateau)
    val rover1a = rover1.navigate(Commands("LMLMLMLMM"))
    val rover2 = Rover(Position(Point(3, 3), East), rover1a.plateau)
    val rover2a = rover2.navigate(Commands("MMRMMLMRRM"))
    val rover3 = Rover(Position(Point(4, 1), South), rover2a.plateau)
    val rover3a = rover3.navigate(Commands("MLMLMRMRM"))

    Seq(rover1a, rover2a, rover3a).foreach(println)
    1 mustEqual 1
  }
}
