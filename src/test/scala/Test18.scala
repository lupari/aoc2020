import challenge.Day18
import org.scalatest._

class Test18 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day18.partOne() should be(21022630974613L)
    Day18.partTwo() should be(169899524778212L)
  }
}
