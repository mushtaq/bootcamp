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

case class Rover(x: Int, y: Int, direction: Direction) {
  def change(command: Char) = command match {
    case 'L' => Rover(x, y, direction.left)
    case 'R' => Rover(x, y, direction.right)
    case 'M' => Rover(x + direction.dx, y + direction.dy, direction)
  }

  def move(commands: String) = commands.foldLeft(this)(_ change _)
}
