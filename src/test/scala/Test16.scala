import challenge.Day16
import org.scalatest._

class Test16 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day16.partOne() should be(21071)
    Day16.partTwo() should be(3429967441937L)
  }
}
