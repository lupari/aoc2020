import challenge.Day09
import org.scalatest._

class Test09 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day09.partOne() should be(138879426)
    Day09.partTwo() should be(23761694)
  }
}
