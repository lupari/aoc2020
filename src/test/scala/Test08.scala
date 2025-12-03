import challenge.Day08
import org.scalatest._

class Test08 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day08.partOne() should be(1744)
    Day08.partTwo() should be(1174)
  }
}
