package fp

trait RNG {


  def nextInt: (Int, RNG)

  def randomPair(rng: RNG): (Int,Int) = {
    val (i1,_) = rng.nextInt
    val (i2,_) = rng.nextInt
    (i1,i2)
  }


}





case class SimpleRNG(seed:Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG{
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, ra2) = ra(rng)
      val (b, rb2) = rb(ra2)
      (f(a,b), rb2)
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, ra) = f(rng)
      g(a)(ra)
    }

  def mapF[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s){ a =>
      rnd => (f(a), rnd)
    }

  def mapF2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a =>
      flatMap(rb){ b =>
        rnd => (f(a,b),rnd)
      }
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2:  Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble/Int.MaxValue.toDouble)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) { rnd =>
        (mod, rnd)
      }
      else nonNegativeLessThan(n)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1,ns) = rng.nextInt
    if(i1 == Int.MaxValue)(0, ns)
    else if(i1 < 0)(-i1, ns)
    else (i1, ns)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i1,ns) = nonNegativeInt(rng)
    (i1.toDouble/Int.MaxValue.toDouble, ns)
  }

  def sequence[A](xs: List[Rand[A]]): Rand[List[A]] = rnd => {
    val rarg = xs.foldRight( ( rnd, List.empty[A])){ case (x, b) =>
      val (c, rc) = x(b._1)
      (rc, c :: b._2)
    }
    (rarg._2, rarg._1)
  }

  def ints3(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence[Int]( List.fill(count)( rnd2 => {
      rnd2.nextInt
    }))(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1,ns) = rng.nextInt
    val (d1,ns2) =  double(ns)
    ((i1, d1), ns2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d1,ns) =  double(rng)
    val (i1,ns2) = ns.nextInt
    ((d1, i1), ns2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,ns1) =  double(rng)
    val (d2,ns2) =  double(ns1)
    val (d3,ns3) =  double(ns2)
    ((d1, d2, d3),ns3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(r: RNG)(count: Int, xs: List[Int]): (List[Int], RNG) =
      if(count == 0)(xs, r)
      else {
        val (i, n) = r.nextInt
        loop(n)(count-1, i :: xs)
      }
    loop(rng)(count, Nil)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, xs:(List[Int], RNG)): (List[Int], RNG) =
      if(count == 0)xs
      else {
        val (i, n) = xs._2.nextInt
        loop(count-1, (i :: xs._1, n))
      }
    loop(count, (Nil, rng) )
  }
}


object State{
  type State[S,+A] = S => (A,S)

  type Rand[A] = State[RNG, A]

  def sequence[A,S](xs: List[S => (A,S)]): S => (List[A], S) = rng => {
    val rarg = xs.foldRight( ( rng, List.empty[A])){ case (x, b) =>
      val (c, rc) = x(b._1)
      (rc, c :: b._2)
    }
    (rarg._2, rarg._1)
  }
}

case class State[S,+A](run: S => (A,S)){
  def unit: State[S, A] = State(run)

  def map[B](f: A => B): S => (B,S) =
    rng => {
      val (a, rng2) = run(rng)
      (f(a), rng2)
    }

  def map2[B,C](rb: A => (B,S))(f: (A, B) => C): S => (C, S) =
    rng => {
      val (a, _) = run(rng)
      val (b, rb2) = rb(a)
      (f(a,b), rb2)
    }

  def flatMap[B](g: A => (S => (B,S))): S => (B,S) =
    rng => {
      val (a, ra) = run(rng)
      g(a)(ra)
    }




  // def sequence(xs: List[])

}
