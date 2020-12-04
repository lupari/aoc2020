import challenge.{Day04, Day04b}
import org.scalatest._

class Test04 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day04.run() should be(250)
    Day04b.run() should be(158)
  }

}
