import challenge.Day22
import org.scalatest._

class Test22 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day22.partOne() should be(35202)
    Day22.partTwo() should be(32317)
  }
}
