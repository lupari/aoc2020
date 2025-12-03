import challenge.Day05
import org.scalatest._

class Test05 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day05.partOne() should be(965)
    Day05.partTwo() should be(524)
  }

}
