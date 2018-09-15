package io.surfkit.printable

trait Printable[A]{
  def format(x: A): String
}

object PrintableInstances{
  implicit val printableString: Printable[String] = new Printable[String]{
    def format(x: String): String = x
  }

  implicit val printableInt: Printable[Int] = new Printable[Int]{
    def format(x: Int): String = x.toString
  }
}

object Printable{
  def format[A](x: A)(implicit printable: Printable[A]) = {
    printable.format(x)
  }

  def print[A](x: A)(implicit printable: Printable[A]) = {
    println(printable.format(x))
  }
}

object PrintableSyntax{
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]) = {
      printable.format(value)
    }

    def print(implicit printable: Printable[A]) = {
      println(printable.format(value))
    }
  }
}
