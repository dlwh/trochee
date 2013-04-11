package trochee.codegen

import virtualization.lms.internal._
import trochee.kernels.{KernelOpsExp, KernelOps}
import trochee.util.{NiceNamesGen, CStructExp}

/**
 * This code generator differs from the one in LMS in that it *only* generates kernels.
 * @author dlwh
 */
trait OpenCLKernelGenBase extends GenericFatCodegen with NiceNamesGen {
  val IR: Expressions with Effects with FatExpressions with KernelOpsExp
  import IR._

  override def quote(x: Exp[Any]): String = x match {
    case FieldDeref(sym, field) => preferNoLocal(sym)+"." +field
    case FieldPointerDeref(sym, field) => preferNoLocal(sym)+"->" +field
    case _ => super.quote(x)

  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) {
    rhs match {
      case glob@GlobalId(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_global_id(${quote(id)})")
      case _ => super.emitNode(sym, rhs)
    }

  }

}
