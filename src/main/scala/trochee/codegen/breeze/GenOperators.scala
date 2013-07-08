package trochee.codegen.breeze

import trochee.basic.{ExtraBaseExp, ExtraBase}
import scala.virtualization.lms.common.{BaseExp, Base}
import scala.virtualization.lms.internal.{ScalaFatCodegen, GenericCodegen}
import trochee.util.NiceNamesGen
import java.io.{PrintWriter, StringWriter}
import breeze.linalg.operators.OpType
import trochee.breeze.{DenseVectorBuilderOps, OpGeneratorOps}

/**
 * TODO
 *
 * @author dlwh
 **/
abstract class GenOperators extends GenericCodegen with ScalaFatCodegen  with NiceNamesGen {
  val IR: BaseExp with ExtraBaseExp with scala.virtualization.lms.internal.Expressions with scala.virtualization.lms.internal.Effects with scala.virtualization.lms.internal.FatExpressions with OpGeneratorOps with DenseVectorBuilderOps

  import IR._

  def genBinaryOp[LHS: Manifest, RHS: Manifest, Result: Manifest](name:String, op: OpType)(body: (Rep[LHS], Rep[RHS])=>Rep[Result]) = {
    val lhs = freshMut[LHS]
    val rhs = freshMut[RHS]
    val y = reifyEffects(body(lhs, rhs)) // unfold completely at the definition site.
    val sout = new StringWriter
    val out = new PrintWriter(sout)
    val lhsType = implicitly[Manifest[LHS]]
    val rhsType = implicitly[Manifest[RHS]]
    val resType = implicitly[Manifest[Result]]
    val opType = op.getClass.getName.replaceAll("$", "") // usually an object with the same name as class type
    withStream(out) {
      out.println(s"${addTab}object $name extends BinaryOp[$lhsType, $rhsType, $opType, $resType] {")
      tabWidth += 1
      out.println(s"${addTab}def apply(lhs: $lhsType, rhs: $rhsType): $resType = {")
      tabWidth += 1
      emitBlock(y)
      tabWidth -= 1
      out.println(s"${addTab}}")
      tabWidth -= 1
      out.println(s"${addTab}}")
    }
  }

  private def freshMut[T:Manifest] = {
    val sym = reflectMutableSym(fresh[T])
    sym
  }

}

/*
object MakeOperators {
  def main(args: Array[String]) = {
    val IR = new BaseExp with ExtraBaseExp with DenseVectorBuilderOpsExp
  }
}
*/
