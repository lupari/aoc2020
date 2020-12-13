package lib

import scala.annotation.tailrec
import scala.math.Integral.Implicits._

object Numbers {

  def gcdExt[A](u: A, v: A)(implicit i: Integral[A]): ((A, A), A, (A, A)) = {

    @tailrec
    def aux(s2: A, s: A, t2: A, t: A, r2: A, r: A): ((A, A), A, (A, A)) = {
      if (r2 == 0) ((s, t), r, (s2, t2))
      else {
        val q = r / r2
        aux(s - q * s2, s2, t - q * t2, t2, r - q * r2, r2)
      }
    }
    aux(i.zero, i.one, i.one, i.zero, v, u)
  }

  @tailrec
  def gcd[A: Integral](a: A, b: A): A = if (b == 0) a else gcd(b, a % b)
  def lcm[A: Integral](a: A, b: A): A = a * b / gcd(a, b)

  def crt[A: Integral](xs: Seq[(A, A)]): (A, A) = {
    def _crt(an1: (A, A), an2: (A, A)): (A, A) = {
      val (a1, n1) = an1
      val (a2, n2) = an2
      val (m1, m2) = gcdExt(n1, n2)._1
      val N        = lcm(n1, n2)
      val x        = a1 * m2 * n2 + a2 * m1 * n1
      ((x % N + N) % N, N)
    }
    xs.reduce(_crt)
  }

}
