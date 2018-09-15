package fpscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def tail[A](xs:List[A]) = xs match{
    case Nil => Nil
    case Cons(y, ys) => ys
  }

  def setHead[A](h:A, xs:List[A]) = Cons(h, List.tail(xs))


  def drop[A](l: List[A], n: Int): List[A] =
    if( n == 0 )l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
    case Nil => Nil
    case Cons(x, xs) =>
      if( f(x) )dropWhile(xs, f)
      else Cons(x, xs)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as,Nil:List[B])( (a,b) => {append(f(a), b)} )

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)( (a) => if(f(a)) Cons(a,Nil:List[A]) else Nil:List[A] )

  def addAdj(xs:List[Int],ys:List[Int]):List[Int] =
    (xs,ys) match{
      case (Nil,Nil) => Nil
      case (Cons(a,as),Cons(b,bs)) => Cons(a+b,addAdj(as,bs))
      case _ => Nil
    }

  def zipWith[A,B](xs:List[A],ys:List[A])(f: (A,A) => B):List[B] =
    (xs,ys) match{
      case (Nil,Nil) => Nil:List[B]
      case (Cons(a,as),Cons(b,bs)) => Cons(f(a,b),zipWith(as,bs)(f))
      case _ => Nil
    }

  def append[A](xs:List[A], ys:List[A]):List[A] =
    foldRight(xs,ys)( (b,a) => { Cons(b,a) } )

  def flatten[A](xs:List[List[A]]):List[A] =
    xs match{
      case Nil => Nil
      case Cons(y,ys) => append(y,flatten(ys))
    }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as,Nil:List[B])( (a,b) => {Cons(f(a), b)} )
  }



  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match{
      case Nil => Nil
      case Cons(x,xs) =>
        if( f(x) )Cons(x,filter(xs)(f))
        else filter(xs)(f)
    }

  def plus1(xs:List[Int]):List[Int] =
    foldRight(xs,Nil:List[Int])( (a,b) => {Cons(a+1, b)} )

  def d2s(xs:List[Double]):List[String] =
    foldRight(xs,Nil:List[String])( (a,b) => {Cons(a.toString, b)} )

  def length[A](as: List[A]): Int = foldRight(as,0)( (a,b) => 1 + b )

  def length2[A](as: List[A]): Int = foldLeft(as,0)( (b,a) => 1 + b )

  def reverse[A](xs:List[A]):List[A] =
    foldLeft(xs, Nil:List[A])( (b,a) => { Cons(a,b) } )

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match{
      case Nil => z
      //case Cons(x,xs) => f(foldLeft(xs,z)(f),x)
      case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)
  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def init[A](l: List[A]): List[A] = l match{
    case Nil => Nil
    case Cons(x, xs) =>
      if( xs != Nil)Cons(x, init(xs))
      else Nil
  }



}
