package fp
/**
  * Created by suroot on 14/05/17.
  */
import scala.{Stream => _, _}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


}

sealed trait Stream[+A]{
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  private [this]  def traverse(xs: Stream[A]): List[A] = xs match{
    case Empty => Nil
    case Cons(h, t) => h() :: traverse(t())
  }

  def toList: List[A] = {
    traverse(this)
  }

  def take(n: Int): Stream[A] = {
    def traverse(xs: Stream[A], count: Int): Stream[A] = xs match{
      case Cons(h, t) if count < n => Stream.cons(h(), traverse(t(), count+1))
      case _ => Empty
    }
    traverse(this, 0)
  }

  def drop(n: Int): Stream[A] = {
    def traverse(xs: Stream[A], count: Int): Stream[A] = xs match{
      case Cons(h, t) if count < n => traverse(t(), count+1)
      case xs => xs
    }
    traverse(this, 0)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def traverse(xs: Stream[A]): Stream[A] = xs match{
      case Cons(h, t) if p(h()) => Stream.cons(h(), traverse(t()))
      case _ => Empty
    }
    traverse(this)
  }
}