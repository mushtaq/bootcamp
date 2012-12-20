case class Point(x: Double, y: Double) {def toJson = s"[$x, $y]"}

case class Line(p1: Point, p2: Point) {
  def pointAt(y: Double) = Point((y - c) / m, y)

  private val m = (p2.y - p1.y) / (p2.x - p1.x)
  private val c = p1.y - m * p1.x
}
case class Triangle(top: Point, left: Point, right: Point) {
  def subTriangleAt(height: Double) =
    Triangle(top, leftLine.pointAt(height), rightLine.pointAt(height))

  private val leftLine = Line(top, left)
  private val rightLine = Line(top, right)
  def toJson = s"{ top: ${top.toJson}, left: ${left.toJson}, right: ${right.toJson} }"
}

case class TestType(name: String, proportion: Double)

case class TestCoverage(testTypes: Seq[TestType]) {
  def splitInProportion(height: Double) = accPs map (p => (height * p) / total)

  private val proportions = testTypes.map(_.proportion)
  private val accPs = proportions.scanLeft(0.0)(_ + _).tail
  private val total = proportions.sum
}

case class Pyramid(height: Double, testCoverage: TestCoverage) {
  def triangles = testCoverage.splitInProportion(height) map outerTriangle.subTriangleAt

  private val outerTriangle =
    Triangle(Point(height / 2, 0), Point(0, height), Point(height, height))

  def toJson = {
    val jsonTriangles = triangles map (_.toJson) mkString ("[", ",", "]")
    s"{ height: $height, triangles: $jsonTriangles }"
  }
}
val tc = TestCoverage(Seq(TestType("unit", 1),TestType("integration", 2),TestType("functional", 1)))

val p = Pyramid(500, tc)

p.triangles foreach println
