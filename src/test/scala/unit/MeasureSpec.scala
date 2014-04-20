package unit

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class MeasureSpec extends Spec with ShouldMatchers {

  import Sugar._

  object `testing length` {
    def `test equality` {
      2.feet should be(24.inches)
      2.feet should not be(24.gram)
      2.yards should be(72.inches)
      2.yards should be(6.feet)
    }

    def `test addition` {
      2.feet + 3.feet should be(5.feet)
      2.feet + 12.inches should be(3.feet)
    }
  }

  object `testing weight` {
    def `test equality` {
      2.kilogram should be(2000.gram)
      2.kilogram should not be(2000.feet)
      2.ton should be(200000.gram)
      2.ton should be(200.kilogram)
//      3000.gram isEqualTo 2.yards should be(true) //compile time error

    }

    def `test addition` {
      2.kilogram + 3.kilogram should be(5.kilogram)
      2.kilogram + 3000.gram should be(5.kilogram)
      3000.gram + 2.kilogram should be(5000.gram)
//      3000.gram + 2.yards should be(5000.gram) //compile time error
    }
  }

  object `testing temperature` {
    def `test equality` {
      37.celsius should be(98.6.fahrenheit)
      37.celsius should not be(98.6.gram)
    }
  }
}
