import org.scalatest._
import challenge.Day06

class Test06 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day06.partOne() should be(6457)
    Day06.partTwo() should be(3260)

  }
}
