package trochee.codegen.opencl

import virtualization.lms.common._
import virtualization.lms.internal.{FatExpressions, Effects, Expressions}
import trochee.kernels.KernelOpsExp

/**
 * 
 * @author dlwh
 */
trait OpenCLKernelGenOrderingOps extends CLikeGenOrderingOps with OpenCLKernelGenBase {
  val IR : Expressions with Effects with FatExpressions with OrderingOpsExp with KernelOpsExp
}
