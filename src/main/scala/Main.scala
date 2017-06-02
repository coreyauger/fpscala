

import fpscala._

import Ex._

object Main extends App {
  println("Hello World: " + (args mkString ", "))

  val t = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Branch(Leaf(4),Leaf(5))))
  println("size:" + Tree.size(t))
  println("size:" + Tree.size2(t))

  println("max:" + Tree.maximum(t))
  println("max:" + Tree.maximum2(t))

  println("depth:" + Tree.depth(t))
  println("depth:" + Tree.depth2(t))

  val t2 = Tree.map(t)( (v) =>{ v*2 })
 // val t3 = Tree.map2(t)( (v) =>{ v*2 })

  println("map:" + Tree.maximum(t2))


  val test = None
  println(test.getOrElse("55"))
  println(test.filter( _ => true))


  val o0 = Some(2)
  val n0 = None

  println(s"Option ${o0.map( _ * 2 )}")
  println(s"Option ${n0.map( (a:Int) => { a*2} )}")

  println(s"Option ${o0.filter( (a) => a == 2 )}")
  println(s"Option ${o0.filter( (a) => a == 3 )}")

 // println("map2:" + Tree.maximum(t2))
  /*
  println(Ex.hasSubsequence(List(1,2,3,4),List(1,2,4)))

  val xs = List(1,2,3,4,5)
  val ys = List(6,7,8,9)
  println(List.tail(xs))

  println(List.setHead(9, xs))
  println(List.dropWhile(xs, (a:Int) => {a!=4}))

  println(List.init(xs))

  println(List.foldRight(List(1,2,3), fpscala.Nil:List[Int])(Cons(_,_)))

  println(List.length(List(1,2,3)))

  println(List.sum3(xs))

  println(List.length2(List(1,2,3)))

  println(xs)
  println(List.reverse(xs))

  println(List.append(xs,ys))

  val as = List(List(1,2,3),List(4,5), List(6,7,8))
  println(List.flatten(as))

  println(List.plus1(xs))

  val bs = List(0.1, 0.2, 0.3, 0.4)
  println(List.d2s(bs))

  println(List.map(xs)( (x) => {x*2} ))

  println(List.filter(xs)( (x) => { x%2 == 0 }))

  println(List.flatMap(List(1,2,3))(i => List(i,i)))

  println(List.filter2(xs)( (x) => { x%2 == 0 }))

  println(List.addAdj(List(1,2,3),List(1,2,3)))
*/
}