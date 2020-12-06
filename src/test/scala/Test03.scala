import challenge.Day03
import org.scalatest._

class Test03 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day03.partOne() should be(265)
    Day03.partTwo() should be(3154761400L)
  }

}
