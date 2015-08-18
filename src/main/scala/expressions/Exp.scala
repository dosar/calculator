package expressions

import operations.Operation

trait ExpNode
{
    def result: BigDecimal
}

case class ValueNode(value: BigDecimal) extends ExpNode
{
    override def result = value
}

case class Exp(left: ExpNode, op: Operation, right: ExpNode) extends ExpNode
{
    override def result =
    {
        if(op == null && right == null) left.result
        else op.operate(left.result, right.result)
    }
}

object Exp
{
    val empty = Exp(null, null, null)
}
