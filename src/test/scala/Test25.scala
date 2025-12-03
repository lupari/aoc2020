import challenge.Day25
import org.scalatest._

class Test25 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day25.partOne() should be(11328376)
  }
}
