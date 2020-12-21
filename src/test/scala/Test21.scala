import challenge.Day21
import org.scalatest._

class Test21 extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  it should "calculate correct result" in {
    Day21.partOne() should be(2798)
    Day21.partTwo() should be("gbt,rpj,vdxb,dtb,bqmhk,vqzbq,zqjm,nhjrzzj")
  }
}
