package operations

object OpType extends Enumeration
{
    type OpType = Value
    val Binary, Unary, Both = Value
}

import OpType._

trait Operation
{
    def opType: OpType
    def operate(left: BigDecimal, right: => BigDecimal): BigDecimal
    def >(op: Operation) : Boolean
    def copy(opType: OpType): Operation =
    {
        if(opType == this.opType) this
        else throw new Error(s"not supported from ${this.opType} to $opType")
    }
}

case class MaximumPriorityOpWrapper(op: Operation) extends Operation
{
    override def opType: OpType = op.opType

    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal = op.operate(left, right)

    override def >(anotherOp: Operation): Boolean = true

    override def copy(opType: OpType) = MaximumPriorityOpWrapper(op.copy(opType = opType))
}

import MaximumPriorityOpWrapper._

object MaximumPriorityOpWrapper
{
    def isMaximumPriority(op: Operation) = op match
    {
        case MaximumPriorityOpWrapper(_) => true
        case _ => false
    }
}

case class Plus(opType: OpType = Both) extends Operation
{
    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal =
    {
        if(opType == Binary) left + right
        else throw new Error("op in non determined kind")
    }

    override def >(op: Operation) = false

    override def copy(opType: OpType) = Plus(opType)
}

case class Minus(opType: OpType = Both) extends Operation
{
    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal =
    {
        if(opType == Binary) left - right
        else if(opType == Unary) -left
        else throw new Error("op in non determined kind")
    }

    override def >(op: Operation) = false

    override def copy(opType: OpType) = Minus(opType)
}

case object sin extends Operation
{
    override val opType: OpType = Unary

    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal = scala.math.sin(left.doubleValue())

    override def >(op: Operation) = true
}

case object cos extends Operation
{
    override val opType: OpType = Unary

    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal = scala.math.cos(left.doubleValue())

    override def >(op: Operation) = true
}

case object fact extends Operation
{
    override val opType: OpType = Unary

    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal =
    {
        if(left < 0 || !left.isWhole()) throw new Error("factorial on negative integers or not whole")
        if(left == BigDecimal(0) || left == BigDecimal(1)) 1
        else left * operate(left - 1, right)
    }

    override def >(op: Operation) = true
}

case object ^ extends Operation
{
    override val opType: OpType = Binary

    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal = scala.math.pow(left.doubleValue(), right.doubleValue())

    override def >(op: Operation) = !isMaximumPriority(op)
}

case object * extends Operation
{
    override val opType: OpType = Binary

    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal = left * right

    override def >(op: Operation) = (op != *) && (op != /) && (op != ^) && !isMaximumPriority(op)
}

case object / extends Operation
{
    override val opType: OpType = Binary

    override def operate(left: BigDecimal, right: => BigDecimal): BigDecimal = left / right

    override def >(op: Operation) = * > op
}