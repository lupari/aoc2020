import challenge.Day11
import org.scalatest._

class Test11 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day11.partOne() should be(2476)
    Day11.partTwo() should be(2257)
  }
}
