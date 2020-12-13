import challenge.Day13
import org.scalatest._

class Test13 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day13.partOne() should be(2845)
    Day13.partTwo() should be(487905974205117L)
  }
}
