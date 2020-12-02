import org.scalatest._
import challenge.{Day02, Day02b}

class Test02 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day02.run() should be(600)
    Day02b.run() should be(245)
  }

}
