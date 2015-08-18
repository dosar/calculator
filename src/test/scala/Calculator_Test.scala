import calc.CalculatorImpl
import org.scalatest.{Matchers, FunSuite}

import scala.util.Try

/**
 * @author alespuh
 * @since 24.04.2015
 */
object CalcForTest
{
    val calc = new CalculatorImpl
}

class Calculator_v1_Test extends FunSuite
{
    import CalcForTest.calc._

    test("+1")
    {
        assertResult(4)(compute("1 + 3"))
        assertResult(4)(compute("1+ 3"))
        assertResult(4)(compute("1 +3"))
        assertResult(4)(compute("1+3"))
    }

    test("+2")
    {
        assertResult(4.434521)(compute("1 + 3 + 0.434521"))
        assertResult(4.434521)(compute("1 + 3 +0.434521"))
        assertResult(4.434521)(compute("1 + 3+ 0.434521"))
        assertResult(4.434521)(compute("1+ 3 + 0.434521"))
        assertResult(4.434521)(compute("1+3+0.434521"))
    }

    test("-1")
    {
        assertResult(-2)(compute("1 - 3"))
        assertResult(-2)(compute("1- 3"))
        assertResult(-2)(compute("1 -3"))
        assertResult(-2)(compute("1-3"))
    }

    test("-2")
    {
        assertResult(-124238.0997234)(compute("1 - 3 - 124236.0997234"))
        assertResult(-124238.0997234)(compute("1 -3 -124236.0997234"))
        assertResult(-124238.0997234)(compute("1 -3-124236.0997234"))
        assertResult(-124238.0997234)(compute("1-3-124236.0997234"))
        assertResult(-124238.0997234)(compute("1 -3-124236.0997234"))
    }

    test("+-3")
    {
        assertResult(-124348.1997234)(compute("1 - 3 - 124236.0997234 + 20.9 - 154 + 23"))
        assertResult(-124348.1997234)(compute("1- 3- 124236.0997234 +20.9 -154+23"))
    }
}

class Calculator_v2_Test extends FunSuite with Matchers
{
    import CalcForTest.calc._
    test("*1")
    {
        assertResult(3)(compute("1 * 3"))
        assertResult(3)(compute("1 *3"))
        assertResult(3)(compute("1* 3"))
        assertResult(3)(compute("1*3"))
    }

    test("*2")
    {
        assertResult(1.2792)(compute("1 * 3 * 2 * 0.2132"))
        assertResult(1.2792)(compute("1*3* 2 *0.2132"))
    }

    test("/1")
    {
        assertResult(3)(compute("4.5 / 1.5"))
        assertResult(3)(compute("4.5/ 1.5"))
        assertResult(3)(compute("4.5 /1.5"))
        assertResult(3)(compute("4.5/1.5"))
    }

    test("/2")
    {
        assertResult(6)(compute("18 / 3 / 2 / 0.5"))
        assertResult(6)(compute("18/3/ 2 /0.5"))
    }

    test("^1")
    {
        compute("4.5^1.5") should be (BigDecimal("9.545941546018392") +- BigDecimal("0.000000000000001"))
        compute("4.5^ 1.5") should be (BigDecimal("9.545941546018392") +- BigDecimal("0.000000000000001"))
        compute("4.5 ^1.5") should be (BigDecimal("9.545941546018392") +- BigDecimal("0.000000000000001"))
        compute("4.5 ^ 1.5") should be (BigDecimal("9.545941546018392") +- BigDecimal("0.000000000000001"))
    }

    test("^2")
    {
        compute("0.5 ^ 1.5 ^ 2 ^ 4") should be (BigDecimal(0) +- BigDecimal("0.00000000000000000000000000001"))
        compute("0.5^1.5^ 2 ^4") should be (BigDecimal(0) +- BigDecimal("0.00000000000000000000000000001"))
    }

    test("*/^4")
    {
        compute("1 + 3 * 2 /4 ^2.01 - 20/2.34 ^ 2 ^ 2.4") should be (BigDecimal("1.144777937") +- BigDecimal("0.000000001"))
    }
}

class Calculator_v3_Test extends FunSuite with Matchers
{
    import CalcForTest.calc._

    test("-6/3")
    {
        assertResult(-2)(compute("-6/3"))
    }

    test("-6/(3)")
    {
        assertResult(-2)(compute("-6/(3)"))
    }

    test("-(6)/(3)")
    {
        assertResult(-2)(compute("-(6)/(3)"))
    }

    test("(-(6))/(3)")
    {
        assertResult(-2)(compute("(-(6))/(3)"))
    }

    test("((-(6))/(3))")
    {
        assertResult(-2)(compute("((-(6))/(3))"))
    }

    test("12 + -6/3")
    {
        assertResult(10)(compute("12 + -6/3"))
    }

    test("12 + -6/3/2")
    {
        assertResult(11)(compute("12 + -6/3/2"))
    }

    test("10 + -6/3/(1 + 1)")
    {
        assertResult(9)(compute("10 + -6/3/(1 + 1)"))
    }

    test("1 + 4/(4 + 4) + 2.5")
    {
        assertResult(4)(compute("1 + 4/(4 + 4) + 2.5"))
    }

    test("(4 + 4)")
    {
        assertResult(8)(compute("(4 + 4)"))
    }

    test("(9 - 3 * 2 + 4 /2 )")
    {
        assertResult(5)(compute("(9 - 3 * 2 + 4 /2 )"))
    }

    test("(9 - 3 * (2 + 1 /2))")
    {
        assertResult(1.5)(compute("(9 - 3 * (2 + 1 /2))"))
    }

    test("(4 + 4) * (9 - 3)")
    {
        assertResult(48)(compute("(4 + 4) * (9 - 3)"))
    }

    test("(4 + 4) / (9 - 5/1)")
    {
        assertResult(2)(compute("(4 + 4) / (9 - 5/1)"))
    }

    test("(4 + 4) / (9 - 5/(23 - 22))")
    {
        assertResult(2)(compute("(4 + 4) / (9 - 5/(23 - 22))"))
    }

    test("4 + 4 / (9 - 5/(23 - 22))")
    {
        assertResult(5)(compute("4 + 4 / (9 - 5/(23 - 22))"))
    }

    test("10!")
    {
        Try(assertResult(3628800)(compute("!10"))).getOrElse(assertResult(3628800)(compute("10!")))
        Try(assertResult(3628800)(compute("!(10)"))).getOrElse(assertResult(3628800)(compute("(10)!")))
        Try(assertResult(3628800)(compute("! ( 10 )"))).getOrElse(assertResult(3628800)(compute(" ( 10 ) !")))
    }

    test("sin 13")
    {
        Try
        {
            compute("sin 13") should be (BigDecimal("0.42016703682664") +- BigDecimal("0.00000000000001"))
        } getOrElse
            {
                compute("sin 13") should be (BigDecimal("0.224951054") +- BigDecimal("0.000000001"))
            }
    }

    test("sin(13 + 0)")
    {
        Try
        {
            compute("sin(13 + 0)") should be (BigDecimal("0.42016703682664") +- BigDecimal("0.00000000000001"))
        } getOrElse
            {
                compute("sin(13 + 0)") should be (BigDecimal("0.224951054") +- BigDecimal("0.000000001"))
            }
    }

    test("sin(13)")
    {
        Try
        {
            compute("sin(13)") should be (BigDecimal("0.42016703682664") +- BigDecimal("0.00000000000001"))
        } getOrElse
            {
                compute("sin(13)") should be (BigDecimal("0.224951054") +- BigDecimal("0.000000001"))
            }
    }

    test("-(4 + 4) / (9 - 5/(23 - 22))")
    {
        assertResult(-2)(compute("-(4 + 4) / (9 - 5/(23 - 22))"))
    }

    test("-(4 + 4) / (9 + - 5/(23 - 22))")
    {
        assertResult(-2)(compute("-(4 + 4) / (9 + - 5/(23 - 22))"))
    }

    test("(4 + 4)! / (9 + - 5/(23 - 22))")
    {
        Try(assertResult(10080)(compute("!(4 + 4) / (9 + - 5/(23 - 22))")))
            .getOrElse(assertResult(10080)(compute("(4 + 4)! / (9 + - 5/(23 - 22))")))
    }

    test("10 - 1 / 4! ")
    {
        Try
        {
            compute("10 - 1 / !4 ") should be (BigDecimal("9.95833333333333333") +- BigDecimal("0.00000000000000001"))
        } getOrElse
            {
                compute("10 - 1 / 4! ") should be (BigDecimal("9.95833333333333333") +- BigDecimal("0.00000000000000001"))
            }
    }

    test("10 - 1/ (4 + 4)! / (9 + -5/(23 - 22))")
    {
        Try
        {
            compute("10 - 1/ !(4 + 4) / (9 + -5/(23 - 22))") should be (BigDecimal("9.999993799603174603174") +- BigDecimal("0.000000000000000000001"))
        } getOrElse
            {
                compute("10 - 1/ (4 + 4)! / (9 + -5/(23 - 22))") should be (BigDecimal("9.999993799603174603174") +- BigDecimal("0.000000000000000000001"))
            }
    }

    test("cos(sin(13 - 1/ 1!))")
    {
        Try(compute("cos(sin(13 - 1/ 1!))") should be (BigDecimal("0.8594656272745") +- BigDecimal("0.0000000000001")))
            .orElse(Try(compute("cos(sin(13 - 1/ !1))") should be (BigDecimal("0.8594656272745") +- BigDecimal("0.0000000000001"))))
            .orElse(Try(compute("cos(sin(13 - 1/ 1!))") should be (BigDecimal("0.999993416") +- BigDecimal("0.000000001"))))
            .getOrElse(compute("cos(sin(13 - 1/ 1!))") should be (BigDecimal("0.999993416") +- BigDecimal("0.000000001")))
    }

    test("10 - 1/ !(4 + 4) ^ 0.5")
    {
        Try(compute("10 - 1/ !(4 + 4) ^ 0.5") should be (BigDecimal("9.995019880794440026") +- BigDecimal("0.000000000000000001")))
            .getOrElse(compute("10 - 1/ (4 + 4)! ^ 0.5") should be (BigDecimal("9.995019880794440026") +- BigDecimal("0.000000000000000001")))
    }

    test("10 - 10 / 2/ (9 + -5/(23 - 22)) ^ 2")
    {
        assertResult(9.6875)(compute("10 - 10 / 2/ (9 + -5/(23 - 22)) ^ 2"))
    }

    test("10 - 10 / 2/ (9 + -5/1) ^ 2")
    {
        assertResult(9.6875)(compute("10 - 10 / 2/ (9 + -5/1) ^ 2"))
    }

    test("10 - 5/ (9 + -5/1) ^ 2")
    {
        assertResult(9.6875)(compute("10 - 5/ (9 + -5/1) ^ 2"))
    }

    test("10 - 5/ (9 + -5) ^ 2")
    {
        assertResult(9.6875)(compute("10 - 5/ (9 + -5) ^ 2"))
    }

    test(" - 5/ (9 + -5) ^ 2")
    {
        assertResult(-0.3125)(compute(" - 5/ (9 + -5) ^ 2"))
    }

    test("5/ (9 + -5) ^ 2")
    {
        assertResult(0.3125)(compute("5/ (9 + -5) ^ 2"))
    }

    test("5/ (9 - 5) ^ 2")
    {
        assertResult(0.3125)(compute("5/ (9 -5) ^ 2"))
    }

    test("10 - 5/ (4 ^ 2)")
    {
        assertResult(9.6875)(compute("10 - 5/ (4 ^ 2)"))
    }

    test("10 - 5/ (4) ^ 2")
    {
        assertResult(9.6875)(compute("10 - 5/ (4) ^ 2"))
    }

    test("10 / 2/ (9 ^ 4)")
    {
        compute("10 / 2/ (9 ^ 4)") should be (BigDecimal("0.00076207895137936290") +- BigDecimal("0.00000000000000000001"))
    }

    test("(10 / 2) * 9 ^ 4")
    {
        assertResult(32805)(compute("(10 / 2) * 9 ^ 4"))
    }

    test("10 - (5 / 4 ^ 4)")
    {
        assertResult(9.98046875)(compute("10 - (5 / 4 ^ 4)"))
    }

    test("(10 - 5) / 4 ^ 4 * (2 + 3 - 10 /6) / (2 + -4)")
    {
        compute("(10 - 5) / 4 ^ 4 * (2 + 3 - 10 /6) / (2 + -4)") should be (BigDecimal("-0.032552083333333") +- BigDecimal("0.000000000000001"))
    }

    test("(10 - 5) / 4 ^ 4 * (2 + 3 - 10 /6) + (2 + -4)")
    {
        compute("(10 - 5) / 4 ^ 4 * (2 + 3 - 10 /6) + (2 + -4)") should be (BigDecimal("-1.9348958333333333") +- BigDecimal("0.0000000000000001"))
    }
}
