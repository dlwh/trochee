package trochee.codegen

import virtualization.lms.internal._
import trochee.kernels.{KernelOpsExp, KernelOps}
import trochee.util.{NiceNamesGen, CStructExp}
import virtualization.lms.common._

/**
 * This code generator differs from the one in LMS in that it *only* generates kernels.
 * @author dlwh
 */
trait OpenCLKernelGenBase extends GenericFatCodegen with NiceNamesGen {
  val IR: Expressions with Effects with FatExpressions with KernelOpsExp with VariablesExp
  import IR._

  override def quote(x: Exp[Any]): String = x match {
    case y: Sym[_] => nameForSym(y).getOrElse(super.quote(x))
    case Const(f: Float) => if(f == Float.PositiveInfinity) {
      "INFINITY"
    } else if(f == Float.NegativeInfinity) {
      "-INFINITY"
    } else{
      super.quote(x)
    }
    case FieldDeref(sym, field) => preferNoLocal(sym)+"." +field
    case FieldPointerDeref(sym, field) => preferNoLocal(sym)+"->" +field
    case _ => super.quote(x)

  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) {
    rhs match {
      case glob@GlobalId(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_global_id(${quote(id)})")
      case NewVar(exp) => emitValDef(sym, quote(exp))
      case ReadVar(Variable(_sym)) => emitValDef(sym, quote(_sym))
      case _ => super.emitNode(sym, rhs)
    }

  }

}
