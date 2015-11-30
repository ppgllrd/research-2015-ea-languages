/**
  * Created by Pepe Gallardo on 30/11/2015.
  */


trait Representation[R] {
  def construct(len : Int) : R
  def mutate(xs : R) : R
  def description : String
}

// individuals represented as mutable Arrays of Booleans
case object ArrayRep extends Representation[Array[Boolean]] {
  def construct(len : Int): Array[Boolean] =
    Array.fill(len)(scala.util.Random.nextDouble > 0.5)

  def mutate(xs: Array[Boolean]): Array[Boolean] = {
    val point = scala.util.Random.nextInt(xs.length)
    xs(point) = !xs(point)
    xs
  }

  def description = "Scala-Array"
}

// individuals represented as immutable Vectors of Booleans
case object VectorRep extends Representation[Vector[Boolean]] {
  def construct(len: Int): Vector[Boolean] =
    Vector.fill(len)(scala.util.Random.nextDouble > 0.5)

  def mutate(xs: Vector[Boolean]): Vector[Boolean] = {
    val point = scala.util.Random.nextInt(xs.length)
    xs.updated(point, !xs(point))
  }

  def description = "Scala-Vector"
}


// individuals represented as immutable Strings
case object StringRep extends Representation[String] {
  def construct(len: Int): String =
    scala.util.Random.alphanumeric.take(len).map((a: Char) => if (a > 'M' ) "1" else "0").mkString

  def mutate(xs: String): String = {
    val point = scala.util.Random.nextInt(xs.length)
    xs.updated(point, if(xs(point) == '0') '1' else '0')
  }

  def description = "Scala-String"
}


// individuals represented as Java's mutable BitSets
case object BitSetRep extends Representation[java.util.BitSet] {
  def construct(len: Int) = new java.util.BitSet(len)

  def mutate(xs: java.util.BitSet): java.util.BitSet = {
    val point = scala.util.Random.nextInt(xs.size())
    xs.flip(point)
    xs
  }

  def description: String = "Java-BitSet"
}

object bitflip extends App {

  def test[R](rep: Representation[R]): Unit = {
    val iterations = 100000
    val top_length = 32768
    val repetitions = 20

    var len = 16
    do {
      var times = 0.0
      for (r <- 1 to repetitions) {

        val indi = rep.construct(len)

        val initialTime = System.nanoTime()
        for (i <- 1 until iterations) {
          val newIndi = rep.mutate(indi)
        }
        val finalTime = System.nanoTime()
        val time = finalTime - initialTime
        times += time
      }
      val avgTime = times / repetitions
      println(rep.description + ", " + len + ", " + avgTime / 1e9)
      len = len * 2
    } while (len <= top_length)
  }

  test(ArrayRep)
  test(VectorRep)
  test(BitSetRep)
  test(StringRep)
}

