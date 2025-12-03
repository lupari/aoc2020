import challenge.Day20
import org.scalatest._

class Test20 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day20.partOne() should be(8425574315321L)
    Day20.partTwo() should be(1841)
  }
}
