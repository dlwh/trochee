package trochee.codegen

import scala.virtualization.lms.internal.{GenerationFailedException, FatExpressions, Effects, Expressions}
import trochee.kernels.KernelOpsExp
import trochee.basic.ExtraBaseExp
import scala.virtualization.lms.common.RangeOpsExp

/**
 * 
 * @author dlwh
 */
trait OpenCLKernelGenRangeOps extends OpenCLKernelGenBase {
  val IR:  Expressions with Effects with FatExpressions with ExtraBaseExp with KernelOpsExp with RangeOpsExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) =>
      throw new GenerationFailedException("OpenCLGenRangeOps: Range vector is not supported")
    case RangeForeach(start, end, i, body) =>
      stream.println("for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      emitBlock(body)
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }

}
