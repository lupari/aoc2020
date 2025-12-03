import challenge.Day15
import org.scalatest._

class Test15 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day15.partOne() should be(257)
    Day15.partTwo() should be(8546398)
  }
}
