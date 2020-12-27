package challenge

object Day25 {

  def transform(n: Long, sn: Long): Long = sn * n % 20201227

  def loopSize(pbk: Int): Int =
    Iterator.iterate((1L, 0))(i => (transform(i._1, 7L), i._2 + 1)).find(_._1 == pbk).get._2

  def encrypt(sn: Int, loops: Int): Long = (1 to loops).foldLeft(1L)((a, _) => transform(a, sn))

  def partOne(): Long = {
    val (pbk1, pbk2) = (10705932, 12301431)
    encrypt(pbk1, loopSize(pbk2))
  }

}
