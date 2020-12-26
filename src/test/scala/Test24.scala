import challenge.Day24
import org.scalatest._

class Test24 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day24.partOne() should be(230)
    Day24.partTwo() should be(3565)
  }
}
