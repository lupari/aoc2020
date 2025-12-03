import challenge.Day07
import org.scalatest._

class Test07 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day07.partOne() should be(119)
    Day07.partTwo() should be(155802)
  }
}
