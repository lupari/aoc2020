import challenge.Day04
import org.scalatest._

class Test04 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day04.partOne() should be(250)
    Day04.partTwo() should be(158)
  }

}
