import org.scalatest._
import challenge.{Day01, Day01b}

class Test01 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day01.run() should be(1006176)
    Day01b.run() should be(199132160)
  }
}
