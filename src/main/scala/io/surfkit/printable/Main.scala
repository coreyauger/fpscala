package io.surfkit.printable

import PrintableInstances._


final case class Cat(name: String, age: Int, color: String)

object Cat{
  implicit val printableCat: Printable[Cat] = new Printable[Cat]{
    def format(x: Cat): String = {
      val name = Printable.format(x.name)
      val age = Printable.format(x.age)
      val color = Printable.format(x.color)
      s"$name is a $age year-old $color cat."
    }
  }
}

object Main extends App {

  val cat = Cat("meemoo", 1, "orange")


  Printable.print(cat)

  {
    import PrintableSyntax._
    cat.print
  }

}