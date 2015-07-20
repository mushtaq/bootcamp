package mars

abstract class Direction(val dx: Int, val dy: Int, _left: => Direction, _right: => Direction) {
  lazy val left = _left
  lazy val right = _right
}

object Direction {
  case object East extends Direction(1, 0, North, South)
  case object West extends Direction(-1, 0, South, North)
  case object North extends Direction(0, 1, West, East)
  case object South extends Direction(0, -1, East, West)
}

case class Point(x: Int, y: Int) {
  def <=(other: Point) = x <= other.x && y <= other.y
  def >=(other: Point) = x >= other.x && y >= other.y
  def moveIn(direction: Direction) = Point(x + direction.dx, y + direction.dy)
}

case class Position(point: Point, direction: Direction) {
  def change(command: Char) = command match {
    case 'L' => Position(point, direction.left)
    case 'R' => Position(point, direction.right)
    case 'M' => Position(point.moveIn(direction), direction)
    case  c  => throw new RuntimeException(s"Invalid command: $c")
  }
}

case class Plateau(bottomLeft: Point, topRight: Point, beacons: Set[Point]) {

  def include(point: Point) = {
    val newBeacons = if (contains(point)) beacons else beacons + point
    Plateau(bottomLeft, topRight, newBeacons)
  }

  def shouldMove(currentPoint: Point, nextPoint: Point) =
    contains(currentPoint) && !beacons.contains(nextPoint)

  private def contains(point: Point) = bottomLeft <= point && topRight >= point
}

case class Rover(position: Position, plateau: Plateau) {
  def navigate(commands: Commands) = {
    val position = commands.positionOf(this)
    val newPlateau = plateau.include(position.point)
    Rover(position, newPlateau)
  }
}

case class Commands(commands: String) {
  def positionOf(rover: Rover) =
    commands.foldLeft(rover.position){ (currentPosition, command) =>
      val nextPosition = currentPosition.change(command)
      val shouldMove = rover.plateau.shouldMove(currentPosition.point, nextPosition.point)
      if (shouldMove) nextPosition else currentPosition
    }
}
