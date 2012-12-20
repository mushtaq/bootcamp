case class TestType(name: String, proportion: Double)
case class TestCoverage(testTypes: Seq[TestType]) {
  val proportions = testTypes.map(_.proportion)
  val accPs = proportions.scanLeft(0.0)(_ + _).tail
  val total = proportions.sum
  def splitInProportion(height: Double) = accPs map (p => (height * p) / total)
}

val tc = TestCoverage(Seq(TestType("unit", 1),TestType("integration", 2),TestType("functional", 1)))

tc.proportions
tc.accPs
tc.total
tc.splitInProportion(500)















