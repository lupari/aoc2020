import challenge.{Day03, Day03b}
import org.scalatest._

class Test03 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day03.run() should be(265)
    Day03b.run() should be(3154761400l)

  }

}
