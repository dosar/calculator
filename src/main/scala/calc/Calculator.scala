package calc

import expressions.{ValueNode, ExpNode, Exp}
import operations._
import operations.OpType._

/**
 * @author alespuh
 * @since 24.04.2015
 */
trait Calculator
{
    def compute(input: String): BigDecimal
}

class CalculatorImpl extends Calculator
{
    override def compute(input: String): BigDecimal = parse(input, Exp.empty).result
    
    def parse(input: String, node: ExpNode): ExpNode = input match
    {
        case "" | null => node
        case str if str(0) == ' ' => parse(str.tail, node)
        case str if str(0) == '(' =>
        {
            val contentWithBraces = getContentWithBraces(str)
            val contentWithoutBraces = getContent(contentWithBraces)
            val subNode = parse(contentWithoutBraces, Exp.empty)
            parse(str.drop(contentWithBraces.length), insertNode(Exp(left = subNode, null, null), node))
        }
        case opRegex(op) => parse(input.drop(op.length), insertOp(getOp(op), node))
        case figureRegex(figure) => parse(input.drop(figure.length), insertNode(ValueNode(BigDecimal(figure)), node))
        case _ => throw new Error("unknown operation starting from '" + input + "'")
    }

    def insertNode(nodeToInsert: ExpNode, toNode: ExpNode): ExpNode = toNode match
    {
        case exp @ Exp(null, null, null) => nodeToInsert match
        {
            case vn @ ValueNode(_) => exp.copy(left = vn)
            case _ => exp.copy(left = nodeToInsert)
        }
        case exp @ Exp(null, op, null)   => exp.copy(left = nodeToInsert, op = op.copy(opType = Unary))
        case exp @ Exp(left, null, null) => throw new Error("malformed string insertNode")
        case exp @ Exp(left, op, null) if op.opType == Binary => exp.copy(right = nodeToInsert)
        case exp @ Exp(left, op, right)  => exp.copy(left, op, insertNode(nodeToInsert, right))
    }

    def insertOp(opToInsert: Operation, node: ExpNode): ExpNode = opToInsert.opType match
    {
        case Binary => node match
        {
            case Exp(null, null, null)       => throw new Error("malformed string insertOp bin (null, null, null)")
            case Exp(null, op, null)         => throw new Error("malformed string insertOp bin (null, op, null)")
            case exp @ Exp(left, null, null) => exp.copy(op = opToInsert)
            case exp @ Exp(left, op, null) if op.opType == Unary  => Exp(exp, opToInsert, null)
            case exp @ Exp(left, op, null)   => throw new Error("malformed string insertOp bin (left, op, null)")
            case exp @ Exp(left, op, right) if opToInsert > op => exp.copy(right = insertOp(opToInsert, right))
            case exp @ Exp(left, op, right) => Exp(exp, opToInsert, null)
            case vn @ ValueNode(_) => Exp(vn, opToInsert, null)
            case _ => throw new Error("malformed string insertOp bin match")
        }
        case Unary => node match
        {
            case exp @ Exp(null, null, null) => exp.copy(op = opToInsert)
            case exp @ Exp(null, op, null) if op.opType == Unary => exp.copy(left = Exp(null, opToInsert, null))
            case exp @ Exp(null, op, null)   => throw new Error("malformed string insertOp un (null, null, null)")
            case exp @ Exp(left, null, null) => throw new Error("malformed string insertOp un (left, null, null)")
            case exp @ Exp(left, op, null) if op.opType == Binary => exp.copy(right = Exp(null, opToInsert, null))
            case exp @ Exp(left, op, null)   => throw new Error("malformed string insertOp un (left, op, null)")
            case exp @ Exp(left, op, right)  => exp.copy(right = insertOp(opToInsert, right))
            case vn @ ValueNode(_) => Exp(vn, opToInsert, null)
        }
        case Both => node match
        {
            case exp @ Exp(null, null, null) => insertOp(opToInsert.copy(opType = Unary), node)
            case exp @ Exp(null, op, null) if op.opType == Unary => insertOp(opToInsert.copy(opType = Unary), node)
            case exp @ Exp(null, op, null)   => throw new Error("malformed string insertOp both (null, op, null)")
            case exp @ Exp(left, null, null) => insertOp(opToInsert.copy(opType = Binary), node)
            case exp @ Exp(left, op, null) if op.opType == Unary => insertOp(opToInsert.copy(opType = Binary), node)
            case exp @ Exp(left, op, null) if op.opType == Binary => insertOp(opToInsert.copy(opType = Unary), node)
            case exp @ Exp(left, op, null)   => throw new Error("malformed string insertOp both (left, op, null)")
            case exp @ Exp(left, op, right)  => insertOp(opToInsert.copy(opType = Binary), node)
            case vn @ ValueNode(_) => Exp(vn, opToInsert, null)
        }
    }

    def getContent(contentWithBraces: String): String =
    {
        if(contentWithBraces.length == 2) throw new Error("malformed braces expression")
        contentWithBraces.slice(1, contentWithBraces.length - 1)
    }

    def getContentWithBraces(str: String): String =
    {
        def content(counter: Int, strToExtract: List[Char]): List[Char] = strToExtract match
        {
            case '(' :: tail => '(' :: content(counter + 1, tail)
            case ')' :: tail if counter == 1 => ')' :: Nil
            case ')' :: tail => ')' :: content(counter - 1, tail)
            case ch :: tail => ch :: content(counter, tail)
            case _ => throw new Error("malformed braces expression (match)")
        }

        content(0, str.toList).mkString
    }

    def getOp(op: String): Operation = op match
    {
        case "+" => Plus()
        case "-" => Minus()
        case "*" => *
        case "/" => /
        case "sin" => sin
        case "cos" => cos
        case "!" => fact
        case "^" => ^
        case _ => throw new Error(s"can't understand op '$op'")
    }

    val opRegex = """^([^\s\d\(\)]+).*""".r
    val figureRegex = """^([\d\.]+).*""".r
}