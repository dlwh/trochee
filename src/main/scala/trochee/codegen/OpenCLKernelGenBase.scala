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
      case glob@GlobalSize(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_global_size(${quote(id)})")
      case glob@GlobalOffset(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_global_offset(${quote(id)})")
      case glob@GroupId(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_group_id(${quote(id)})")
      case glob@GroupSize(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_group_size(${quote(id)})")
      case glob@NumGroups(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_num_groups(${quote(id)})")
      case glob@LocalId(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_local_id(${quote(id)})")
      case glob@LocalSize(id) => emitValDef(sym.withPos(List(glob.pos)), s"get_local_size(${quote(id)})")
      case Barrier(BarrierType.Global) => emitValDef(sym, s"barrier(CLK_GLOBAL_MEM_FENCE)")
      case Barrier(BarrierType.Local) => emitValDef(sym, s"barrier(CLK_LOCAL_MEM_FENCE)")
      case Barrier(BarrierType.Both) => emitValDef(sym, s"barrier(CLK_LOCAL_MEM_FENCE|CLK_GLOBAL_MEM_FENCE)")
      case MemFence(BarrierType.Global) => emitValDef(sym, s"mem_fence(CLK_GLOBAL_MEM_FENCE)")
      case MemFence(BarrierType.Local) => emitValDef(sym, s"mem_fence(CLK_LOCAL_MEM_FENCE)")
      case MemFence(BarrierType.Both) => emitValDef(sym, s"mem_fence(CLK_LOCAL_MEM_FENCE|CLK_GLOBAL_MEM_FENCE)")
      case ReadFence(BarrierType.Global) => emitValDef(sym, s"read_mem_fence(CLK_GLOBAL_MEM_FENCE)")
      case ReadFence(BarrierType.Local) => emitValDef(sym, s"read_mem_fence(CLK_LOCAL_MEM_FENCE)")
      case ReadFence(BarrierType.Both) => emitValDef(sym, s"read_mem_fence(CLK_LOCAL_MEM_FENCE|CLK_GLOBAL_MEM_FENCE)")
      case WriteFence(BarrierType.Global) => emitValDef(sym, s"write_mem_fence(CLK_GLOBAL_MEM_FENCE)")
      case WriteFence(BarrierType.Local) => emitValDef(sym, s"write_mem_fence(CLK_LOCAL_MEM_FENCE)")
      case WriteFence(BarrierType.Both) => emitValDef(sym, s"write_mem_fence(CLK_LOCAL_MEM_FENCE|CLK_GLOBAL_MEM_FENCE)")
      case NewVar(exp) => emitValDef(sym, quote(exp))
      case ReadVar(Variable(_sym)) => emitValDef(sym, quote(_sym))
      case Equals(x, y) => emitValDef(sym, s"${quote(x)} == ${quote(y)}")
      case NotEquals(x, y) => emitValDef(sym, s"${quote(x)} != ${quote(y)}")
      case _ => super.emitNode(sym, rhs)
    }

  }

}
