import challenge.{Day11, Day12}
import org.scalatest._

class Test12 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day12.partOne() should be(1603)
    Day12.partTwo() should be(52866)
  }
}
