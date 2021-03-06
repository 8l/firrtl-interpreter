/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/
package firrtl_interpreter

import firrtl_interpreter.TestUtils._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by chick on 4/27/16.
  */
class ConcreteSpec extends FlatSpec with Matchers {
  val maxWidth = TestUtils.MaxWidth

  behavior of "random BigInt generation"

  it should "not create numbers with wider than specified width" in {
    for(i <- IntWidthTestValuesGenerator(1, maxWidth)) {
      for( trails <- 0 to 200) {
        val x = randomBigInt(i)
//        println(s"$i $x ${x.bitLength}")
        x.bitLength should be <= i
      }
    }
  }

  behavior of "creating concrete values"

  it should "work up to a reasonable size" in {
    for(i <- IntWidthTestValuesGenerator(1, maxWidth)) {
      for( trails <- 0 to 200) {
        val x = randomBigInt(i)
//        println(s"$i $x ${x.bitLength}")

        val ci = ConcreteUInt(x, i)
        ci.isInstanceOf[ConcreteUInt] should be (true)
        ci.width should be <= i
      }
    }
  }

  behavior of "concrete addition"

  it should "return proper type under mixed addition" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 + i2).isInstanceOf[ConcreteUInt] should be (true)
    (i1 + s2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 + i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 + s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 + i2).width should be (i1.width.max(i2.width)+1)
    (i1 + s2).width should be (i1.width.max(s2.width)+1)
    (s1 + i2).width should be (s1.width.max(i2.width)+1)
    (s1 + s2).width should be (s1.width.max(s2.width)+1)
  }

  it should "return obvious additions under large range of numbers" in {
    for(width1 <- IntWidthTestValuesGenerator(1, MaxWidth)) {
      for(width2 <- IntWidthTestValuesGenerator(1, MaxWidth)) {
        val (num1, num2) = (randomBigInt(width1), randomBigInt(width2))
        val (cu1, cu2) = (ConcreteUInt(num1, width1), ConcreteUInt(num2, width2))
        val sum = cu1 + cu2

        sum.value should be (cu1.value + cu2.value)
        sum.width should be (width1.max(width2) + 1)
      }
    }
  }

  behavior of "concrete subtraction"

  it should "return proper type under mixed subtraction" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 - i2).isInstanceOf[ConcreteSInt] should be (true)
    (i1 - s1).isInstanceOf[ConcreteSInt] should be (true)
    (s1 - i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 - s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 - i2).width should be (i1.width.max(i2.width)+1)
    (i1 - s2).width should be (i1.width.max(s2.width)+1)
    (s1 - i2).width should be (s1.width.max(i2.width)+1)
    (s1 - s2).width should be (s1.width.max(s2.width)+1)
  }

  it should "return obvious subtractions under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (cu1, cu2) = (randC, randC)
      val sum = cu1 - cu2

      sum.value should be (cu1.value - cu2.value)
    }
  }

  behavior of "concrete multiplication"

  it should "return proper type under mixed multiplication" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 * i2).isInstanceOf[ConcreteUInt] should be (true)
    (i1 * s1).isInstanceOf[ConcreteSInt] should be (true)
    (s1 * i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 * s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 * i2).width should be (i1.width + i2.width)
    (i1 * s2).width should be (i1.width + s2.width)
    (s1 * i2).width should be (s1.width + i2.width)
    (s1 * s2).width should be (s1.width + s2.width)
  }

  it should "return obvious multiplications under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (cu1, cu2) = (randC, randC)
      val sum = cu1 * cu2

      sum.value should be (cu1.value * cu2.value)
    }
  }

  behavior of "concrete division"

  it should "return proper type under mixed division" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 / i2).isInstanceOf[ConcreteUInt] should be (true)
    (i1 / s1).isInstanceOf[ConcreteSInt] should be (true)
    (s1 / i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 / s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 / i2).width should be (i1.width)
    (i1 / s2).width should be (i1.width+1)
    (s1 / i2).width should be (s1.width)
    (s1 / s2).width should be (s1.width+1)
  }

  it should "return obvious divisions under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (cu1, cu2) = (randC, randC)

      val dividend = cu1 / cu2
      if(cu2.value != Big0 ) {
        dividend.value should be (cu1.value / cu2.value)
      }
      else {
        dividend.poisoned should be (true)
      }
    }
  }

  behavior of "concrete modulus"

  it should "return proper type under mixed modulus" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 % i2).isInstanceOf[ConcreteUInt] should be (true)
    (i1 % s1).isInstanceOf[ConcreteUInt] should be (true)
    (s1 % i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 % s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 % i2).width should be (i1.width.min(i2.width))
    (i1 % s2).width should be (i1.width.min(s2.width))
    (s1 % i2).width should be (s1.width.min(i2.width+1))
    (s1 % s2).width should be (s1.width.min(s2.width))
  }

  it should "return obvious modulus under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (cu1, cu2) = (randC, randC)

      val modulus = cu1 % cu2
      if(cu2.value != Big0 ) {
        modulus.value should be (cu1.value % cu2.value)
        modulus.poisoned should be (false)
      }
      else {
        modulus.poisoned should be (true)
      }
    }
  }

  behavior of "not"

  it should "flip bits of UInts" in {
    for(width <- IntWidthTestValuesGenerator(-MaxWidth, MaxWidth)) {
      if (width < -1 || width > 1) {
        val bitString1 = (0 until width.abs-1).map(x => (x % 2).toString).mkString
        val bitString2 = (0 until width.abs-1).map(x => ((x+1) % 2).toString).mkString

        for(sign <- Array(-1, 1)) {
          val bigInt = sign*BigInt(bitString1, 2)
          val si = ConcreteSInt(bigInt, width.abs)
          val topUintBit1 = if (bigInt < 0) "1" else "0"
          val topUintBit2 = if (bigInt < 0) "0" else "1"

          val ui       = ConcreteUInt(BigInt(topUintBit1 + bitString1, 2), width.abs)
          val expected = ConcreteUInt(BigInt(topUintBit2 + bitString2, 2), width.abs)

//          println(s"width $width si     ${si.asBinaryString}")
//          println(s"width $width ui     ${ui.asBinaryString}")
//          println(s"width $width si.ex  ${expected.asBinaryString}")
//          println(s"width $width si.not ${si.not.asBinaryString}")
//          println()

          si.not should be (expected)
          ui.not should be (expected)
        }
      }
    }
  }
  behavior of "bits"

  it should "allows arbitrary selection of bits" in {
    def testBits(width: Int, lo: Int, hi: Int) {
      val num = allOnes(width)
      val uint = ConcreteUInt(num, width)

//      println(s"width $width lo $lo hi $hi uint $uint ")
      val subUint = uint.bits(hi, lo)

      val size = hi - lo + 1
      val expected = allOnes(size)
      subUint.value should be (expected)
      subUint.width should be (size)
    }

    testBits(7, 4, 6)

    for(width <- IntWidthTestValuesGenerator(1, maxWidth)) {
      for(lo <- IntWidthTestValuesGenerator(0, width-1)) {
        for(hi <- IntWidthTestValuesGenerator(lo, width-1)) {
          testBits(width, lo, hi)
        }
      }
    }
  }

  behavior of "head"

  it should "allows arbitrary selection of top n bits" in {
    def testBits(width: Int, n: Int) {
      val num = allOnes(width)
      val uint = ConcreteUInt(num, width)

      val subUint = uint.head(n)
      // println(s"width $width n $n uint $uint => $subUint")

      subUint.value should be (allOnes(n))
      subUint.width should be (n)
    }

    for(width <- 1 to maxWidth) {
      for(n <- 1 to width) {
        testBits(width, n)
      }
    }
  }

  behavior of "tail"

  it should "allow arbitrary selection of top n bits" in {
    def testBits(width: Int, n: Int) {
      val num = allOnes(width)
      val uint = ConcreteUInt(num, width)

      val subUint = uint.tail(n)
      //println(s"width $width n $n uint $uint => $subUint")

      val expectedValue = allOnes(width-n)
      subUint.value should be (expectedValue)
      subUint.width should be (width-n)
    }

    for(width <- IntWidthTestValuesGenerator(1, maxWidth)) {
      for(n <- IntWidthTestValuesGenerator(0, width-1)) {
        testBits(width, n)
      }
    }
  }

  it should "satisfy the following specific tests" in {
    val b2 = ConcreteSInt(BigInt(-2), 16)

    b2.tail(1).value should be (32766)
    b2.tail(13).value should be (6)
    b2.tail(14).value should be (2)
    b2.tail(15).value should be (0)
  }

  it should "take an SInt"

  behavior of "dyn shift left"

  it should "work for wide range of values" in {
    def testShiftOp(width: Int, shift: Int): Unit = {
      val num = allOnes(width)
      val shiftedNum = num << shift

      val target = ConcreteUInt(num, width)
      val shiftArg = ConcreteUInt(shift, requiredBits(shift))

      requiredBits(num) should be (width)
      requiredBits(shiftedNum) should be (width + shift)

      val result = target << shiftArg
      // println(s"width $width => num $num arg $shift, target $target result $result")
      result.value should be (shiftedNum)
    }
    testShiftOp(29, 1)

    for(i <- 3 to maxWidth) {
      for(arg <- 1 until i) {
        testShiftOp(i, arg)
      }
    }
  }

  behavior of "SInt with width 1"

  it should "act as either -1 or 0" in {
    val s0_1      = ConcreteSInt(0, 1)
    val sMinus1_1 = ConcreteSInt(-1, 1)
    val s7_16     = ConcreteSInt(7, 16)

    (s7_16 + s0_1).value should be (7)
    (s7_16 * s0_1).value should be (0)
    (s7_16 - s0_1).value should be (7)
//    (s7_16 / s0_1).value should be (0) TODO: what should this be?
//    (s7_16 % s0_1).value should be (0)

    (s7_16 + sMinus1_1).value should be (6)
    (s7_16 * sMinus1_1).value should be (-7)
    (s7_16 - sMinus1_1).value should be (8)
    (s7_16 / sMinus1_1).value should be (-7)
    (s7_16 % sMinus1_1).value should be (0)

    intercept[InterpreterException] {
      ConcreteSInt(1, 1)
    }
  }

  def randC: Concrete = {
    if(random.nextBoolean()) randU else randS
  }
  def randU: ConcreteUInt = {
    val randomWidth = random.nextInt(maxWidth)
    ConcreteUInt(BigInt(randomWidth, random), randomWidth)
  }
  def randS: ConcreteSInt = {
    val randomWidth = random.nextInt(maxWidth)
    ConcreteSInt(BigInt(randomWidth, random) * (if(random.nextBoolean()) 1 else -1), randomWidth+1)
  }
}
