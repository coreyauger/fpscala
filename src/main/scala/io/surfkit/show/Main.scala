package io.surfkit.show

import cats._
import cats.implicits._


final case class Cat(name: String, age: Int, color: String)

object Cat{
  implicit val printableCat: Show[Cat] = Show.show{ x =>
    val name = x.name.show
    val age = x.age.show
    val color =  x.color.show
    s"$name is a $age year-old $color cat."
  }

  implicit val catsEq = Eq.instance[Cat]{ (cat1, cat2) =>
    cat1 == cat2
  }
}

object Main extends App {

  val cat = Cat("meemoo", 1, "orange")

  println(cat.show)


  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat1)
  println(cat1 === cat2)

}