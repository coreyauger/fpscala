package fpscala

sealed trait Tree[+A]{

}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

  def fold[A,B](t: Tree[A])(z: (A) => B)(f: (B, B) => B): B = {
    t match{
      case Branch(l, r) =>
        f(fold(l)(z)(f),fold(r)(z)(f))
      case Leaf(v) =>
        z(v)
    }
  }

  def size2[A](t:Tree[A]):Int =
    fold(t)( (v) => { 1 })( (a,b) => 1+a+b )


  def size[A](t:Tree[A]):Int = {
    t match{
      case Branch(l, r) =>
        1 + size(l) + size(r)
      case Leaf(_) =>
        1
    }
  }

  def maximum2(t:Tree[Int]):Int =
    fold(t)( (v) => v)( (a,b) => a.max(b) )

  def maximum(t:Tree[Int]):Int = {
    t match{
      case Branch(l, r) =>
        maximum(l).max(maximum(r))
      case Leaf(v) =>
        v
    }
  }

  def depth2[A](t:Tree[Int],d:Int = 0):Int =
    fold(t)( (v) => d)( (a,b) => (a+1).max((b+1)) )


  def depth[A](t:Tree[Int],d:Int = 0):Int = {
    t match{
      case Branch(l, r) =>
        depth(l,d+1).max(depth(r,d+1))
      case Leaf(v) =>
        d
    }
  }

//  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
//    fold(t)( (v) => Leaf(f(v)))( (a,b) => Branch(a,b) )

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match{
      case Branch(l, r) =>
        Branch(map(l)(f),map(r)(f))
      case Leaf(v) =>
        Leaf(f(v))
    }
  }

}
