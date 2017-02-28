package common


sealed trait VariableType {
  def unary_+ : RepeatedVariableType =
    RepeatedVariableType(this)

  def |(second: VariableType): ConjunctedVariableType =
    ConjunctedVariableType(Seq(this, second))

  def isRightType(concrete: BaseVariableType): Boolean = {
    this match {
      case RepeatedVariableType(childType) => childType.isRightType(concrete)
      case ConjunctedVariableType(types) => types.exists(variableType => variableType.isRightType(concrete))
      case varType: BaseVariableType => varType == concrete
    }
  }
}

object VariableType {
  def isRightParamsSeq(constraints: Seq[ _ >: VariableType], types: Seq[BaseVariableType]): Boolean = {
    if (constraints.length != types.length && !constraints.last.isInstanceOf[RepeatedVariableType])
      false
    else {
      val constraintsExtended: Seq[_ >: VariableType] =
        if (constraints.length != types.length)
          constraints.dropRight(1) ++ (
            Seq.tabulate(types.length - constraints.length + 1)
            (_ => constraints.last))
        else
          constraints
      constraintsExtended.zip(types).forall(t => t._1.asInstanceOf[VariableType].isRightType(t._2))
    }
  }
}

case class RepeatedVariableType(childType: VariableType) extends VariableType

case class ConjunctedVariableType(types: Seq[VariableType]) extends VariableType {
  override def |(second: VariableType): ConjunctedVariableType =
    ConjunctedVariableType(second +: types)
}

object BaseVariableType {
  def apply(t: String): BaseVariableType = t.toLowerCase match {
    case "integer" => Number
    case "boolean" => Boolean
    case "string" => String
  }

  object Number extends BaseVariableType {
    override def toJvmType: String = "I"
  }

  object Boolean extends BaseVariableType {
    override def toJvmType: String = "Z"
  }

  object String extends BaseVariableType {
    override def toJvmType: String = "Ljava/lang/String;"
  }

  object NoType extends BaseVariableType {
    override def toJvmType: String = ""
  }

}

trait BaseVariableType extends VariableType {
  def toJvmType: String
}
