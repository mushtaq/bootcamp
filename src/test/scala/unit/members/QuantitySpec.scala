package unit.members

import org.specs2.mutable.Specification
import unit.members.Sugar._

class QuantitySpec extends Specification {

  "testing length" should {
    "test equality" in {
      2.feet mustEqual 24.inches
      2.feet must not be 24.gram
      2.feet == 24.inches must beTrue
      2.feet == 24.gram must beFalse
      2.feet == 24 must beFalse
      2.feet.in(Length.Inches) mustEqual 24.inches
      2.yards mustEqual 72.inches
      2.yards mustEqual 6.feet
    }

    "test addition" in {
      2.feet + 3.feet mustEqual 5.feet
      2.feet + 12.inches mustEqual 3.feet
    }
  }

  "testing weight" should {
    "test equality" in {
      2.kilogram mustEqual 2000.gram
      2.kilogram must not be 2000.feet
      2.ton mustEqual 200000.gram
      2.ton mustEqual 200.kilogram
//              2.ton.in(Length.Feet) mustEqual 200.kilogram //compile error

    }

    "test addition" in {
      2.kilogram + 3.kilogram mustEqual 5.kilogram
      2.kilogram + 3000.gram mustEqual 5.kilogram
      3000.gram + 2.kilogram + 2.ton mustEqual 205000.gram
//             3000.gram + 2.yards mustEqual(5000.gram) //compile time error
    }
  }

  "testing temperature" should {
    "test equality" in {
      37.celsius mustEqual 98.6.fahrenheit
      37.celsius must not be 98.6.gram
    }
  }
}
