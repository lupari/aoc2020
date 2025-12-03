import challenge.Day14
import org.scalatest._

class Test14 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day14.partOne() should be(11926135976176L)
    Day14.partTwo() should be(4330547254348L)
  }
}
