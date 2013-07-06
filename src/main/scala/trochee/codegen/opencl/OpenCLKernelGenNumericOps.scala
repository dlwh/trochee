package trochee.codegen.opencl

import virtualization.lms.common.{CLikeGenNumericOps, NumericOpsExp, ArrayOpsExp, BaseGenArrayOps}
import virtualization.lms.internal.{FatExpressions, Effects, Expressions}
import trochee.kernels.KernelOpsExp
import trochee.basic.RigOpsExp

/**
 * 
 * @author dlwh
 */
trait OpenCLKernelGenNumericOps extends OpenCLKernelGenBase {
  val IR : Expressions with Effects with FatExpressions with RigOpsExp with KernelOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case o@RigPlus(a,b) =>
          emitValDef(addPos(sym,o), quote(a) + " + " + quote(b))
//        case o@NumericMinus(a,b) =>
//          emitValDef(sym, quote(a) + " - " + quote(b))
        case o@RigTimes(a,b) =>
          emitValDef(addPos(sym, o), quote(a) + " * " + quote(b))
//        case NumericDivide(a,b) =>
//          emitValDef(sym, quote(a) + " / " + quote(b))
        case _ => super.emitNode(sym, rhs)
      }
    }
}
