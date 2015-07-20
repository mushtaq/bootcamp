package unit

object Sugar {

  implicit class SugaredQuantity(val number: Double) extends AnyVal {

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
