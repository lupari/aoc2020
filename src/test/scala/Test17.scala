import challenge.{Day16, Day17}
import org.scalatest._

class Test17 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day17.partOne() should be(276)
    Day17.partTwo() should be(2136)
  }
}
