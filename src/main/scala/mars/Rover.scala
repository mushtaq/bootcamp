package mars

class Direction(val dx: Int, val dy: Int, _left: => Direction, _right: => Direction) {
  lazy val left = _left
  lazy val right = _right
}

object Direction {
  case object East extends Direction(1, 0, North, South)
  case object West extends Direction(-1, 0, South, North)
  case object North extends Direction(0, 1, West, East)
  case object South extends Direction(0, -1, East, West)
}

case class Rover(position: Point, direction: Direction) {
  def change(command: Char) = command match {
    case 'L' => Rover(position, direction.left)
    case 'R' => Rover(position, direction.right)
    case 'M' => Rover(position.moveIn(direction), direction)
  }

  def finalState(commands: String) = commands.foldLeft(this)(_ change _)
}

case class Point(x: Int, y: Int) {
  def <=(other: Point) = x <= other.x && y <= other.y
  def >=(other: Point) = x >= other.x && y >= other.y
  def moveIn(direction: Direction) = Point(x + direction.dx, y + direction.dy)
}

case class Plateau(bottomLeft: Point, topRight: Point, beacons: Set[Rover]) {
  def contains(rover: Rover) = bottomLeft <= rover.position && topRight >= rover.position

  def finalState(rover: Rover, commands: String): Rover =
    commands.foldLeft(rover){ (currentState, command) =>
      val nextState = currentState.change(command)
      val shouldMove = this.contains(currentState) && !beacons.contains(nextState)
      if (shouldMove) nextState else currentState
    }

  def navigate(rover: Rover, commands: String): (Plateau, Rover) = {
    val lastState = finalState(rover, commands)
    val newBeacons = if (this.contains(lastState)) beacons else beacons + lastState
    val newPlateau = Plateau(bottomLeft, topRight, newBeacons)
    (newPlateau, lastState)
  }
}
