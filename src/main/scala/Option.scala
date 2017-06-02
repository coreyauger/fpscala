package fpscala

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}
//import scala.List

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match{
    case Some(v) => Some(f(v))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match{
    case Some(v) => v
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.flatMap( (a) => { if ( a == None)ob else this})

  def filter(f: A => Boolean): Option[A] =
    if(this.map( f ).getOrElse(false)) this else None
}
object Option{
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap( (aa) =>{ b map( (bb) => { f(aa,bb) } ) } )

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    ???
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object test {
  /*
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(Some(Nil) ){ (aa:A,bb:Option[List[A]]) =>
      aa match{
        case Some(a) => Some(a :: bb.getOrElse(Nil))
        case None => None
      }
    } //(f: (B, A) => B): B

  }
  */
}
