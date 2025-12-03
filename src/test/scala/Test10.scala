import challenge.Day10
import org.scalatest._

class Test10 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day10.partOne() should be(2414)
    Day10.partTwo() should be(21156911906816L)
  }
}
