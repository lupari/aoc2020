import challenge.Day23
import org.scalatest._

class Test23 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day23.partOne() should be(98645732)
    Day23.partTwo() should be(689500518476L)
  }
}
