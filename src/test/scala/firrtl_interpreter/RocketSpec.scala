// See LICENSE for license details.
package firrtl_interpreter

import org.scalatest.{Matchers, FlatSpec}

class RocketSpec extends FlatSpec with Matchers {
  behavior of "rocket"

//  it should "parse" in {
//    val firrtlString = io.Source.fromFile("src/test/resources/rocket.fir").mkString
//
//    val x = new InterpretiveTester(firrtlString)
//
//    for(i <- 0 until 10) {
//      x.poke("io_host_in_valid", 1)
//      val ready = x.peek("io_host_in_ready")
//      println(s"ready $ready")
//
//      x.step()
//    }
//    x.report()
//  }
}
