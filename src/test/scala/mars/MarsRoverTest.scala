package mars

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class MarsRoverTest extends Spec with ShouldMatchers{

  import Direction._

  def `rover change` {
    val rover = Rover(3, 3, East)
    rover.change('L') should be(Rover(3, 3, North))
    rover.change('R') should be(Rover(3, 3, South))
    rover.change('M') should be(Rover(4, 3, East))
  }

  def `rover move` {
    val rover = Rover(3, 3, East)
    rover.move("LLLMRR") should be(Rover(3, 2, North))
  }

}
