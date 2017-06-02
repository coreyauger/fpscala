package fp

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1,ns) = rng.nextInt
    if(i1 == Int.MaxValue)(0, ns)
    else if(i1 < 0)(-i1, ns)
    else (i1, ns)
  }

  def randomPair(rng: RNG): (Int,Int) = {
    val (i1,_) = rng.nextInt
    val (i2,_) = rng.nextInt
    (i1,i2)
  }
}


case class SimpleRNG(seed:Long)
  extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


/**
  * Created by suroot on 22/05/17.
  */
class State {

}
