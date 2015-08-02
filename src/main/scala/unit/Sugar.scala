package unit

object Sugar {

  implicit class SugaredIntQuantity(val number1: Int) extends SugaredQuantity(number1)
  implicit class SugaredDoubleQuantity(val number2: Double) extends SugaredQuantity(number2)

  class SugaredQuantity(val number: Double) {

    import Length._

    def inches = Inches(number)
    def feet = Feet(number)
    def yards = Yards(number)

    import Weight._

    def gram = Gram(number)
    def kilogram = Kilogram(number)
    def ton = Ton(number)

    import Temperature._

    def celsius = Celsius(number)
    def fahrenheit = Fahrenheit(number)
  }

}
