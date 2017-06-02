
import scala.List._
import fpscala.Option

import scala.{Option => _, Either => _, _}

object Ex{

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def contains(xs:List[A], ys:List[A]):Boolean = {
      ys match{
        case Nil => true
        case h :: tail =>
          xs match{
            case Nil => false
            case a :: as =>
              if(h == a)contains(as, tail)
              else false
          }
      }
    }
    val as = sup.dropWhile(_ != sub.head)
    println(as)
    if( as == Nil) false
    else {
      val con = contains(as, sub)
      if (con) true
      else hasSubsequence(as.tail, sub)
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) fpscala.None
    else fpscala.Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}