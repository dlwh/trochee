package trochee.codegen.breeze

import trochee.basic._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import trochee.util.NiceNamesGen
import java.io._
import breeze.linalg._
import breeze.linalg.operators._
import trochee.breeze._
import com.thoughtworks.paranamer.AdaptiveParanamer
import trochee.codegen._
import java.util
import trochee.basic.collection._

/**
 * TODO
 *
 * @author dlwh
 **/
abstract class GenOperators extends GenericCodegen with ScalaFatCodegen  with NiceNamesGen {
  val IR: BaseExp with ExtraBaseExp with scala.virtualization.lms.internal.Expressions with scala.virtualization.lms.internal.Effects with scala.virtualization.lms.internal.FatExpressions with OpGeneratorOps with DenseVectorBuilderOps

  import IR._


  private val namedSyms = new util.IdentityHashMap[Sym[_], String]()

  def nameForSym(sym: Sym[_]):Option[String] = {
    Option(namedSyms.get(sym)).orElse(sym.pos.collectFirst { case pos if pos.assignedVariable.nonEmpty => pos.assignedVariable.get}).map(_ + "_" + sym.id)
  }


  private val paranamer = new AdaptiveParanamer()

  override def quote(op: Exp[Any]): String = op match {
    case y: Sym[_] => nameForSym(y).getOrElse(super.quote(op))
    case _ => super.quote(op)
  }

  def genBinaryOp[LHS: Manifest, RHS: Manifest, Result: Manifest](op: VectorBinaryOperator[LHS, RHS, Result]) = {
    val name = op.generatedName
    import op.body
    val args = paranamer.lookupParameterNames(body.getClass.getMethods.find(_.getName == "apply").get)
    val lhs = freshMut[LHS](args(0))
    val rhs = freshMut[RHS](args(1))

    val lhsType = implicitly[Manifest[LHS]]
    val rhsType = implicitly[Manifest[RHS]]
    val resType = implicitly[Manifest[Result]]


    val y = reifyEffects(body(lhs, rhs)) // unfold completely at the definition site.
    val sout = new StringWriter
    val out = new PrintWriter(sout)

    val opType = op.op.getClass.getName.replaceAll("[$]", "") // usually an object with the same name as class type
    withStream(out) {
      out.println(s"${addTab}implicit object $name extends BinaryOp[$lhsType, $rhsType, $opType, $resType] {")
      tabWidth += 1
      out.println(s"${addTab}def apply(${quote(lhs)}: $lhsType, ${quote(rhs)}: $rhsType): $resType = {")
      tabWidth += 1
      emitBlock(y)
      out.println(quote(getBlockResult(y)))
      tabWidth -= 1
      out.println(s"${addTab}}")
      tabWidth -= 1
      out.println(s"${addTab}}")
    }
    out.close()
    sout.toString
  }

    def genBinaryUpdateOp[LHS: Manifest, RHS: Manifest](op: VectorBinaryUpdateOperator[LHS, RHS]) = {
    val name = op.generatedName
    import op.body
    val args = paranamer.lookupParameterNames(body.getClass.getMethods.find(_.getName == "apply").get)
    val lhs = freshMut[LHS](args(0))
    val rhs = freshMut[RHS](args(1))

    val lhsType = implicitly[Manifest[LHS]]
    val rhsType = implicitly[Manifest[RHS]]

    val y = reifyEffects(body(lhs, rhs)) // unfold completely at the definition site.
    val sout = new StringWriter
    val out = new PrintWriter(sout)

    val opType = op.op.getClass.getName.replaceAll("[$]", "") // usually an object with the same name as class type
    withStream(out) {
      out.println(s"${addTab}implicit object $name extends BinaryUpdateOp[$lhsType, $rhsType, $opType] {")
      tabWidth += 1
      out.println(s"${addTab}def apply(${quote(lhs)}: $lhsType, ${quote(rhs)}: $rhsType):Unit = {")
      tabWidth += 1
      emitBlock(y)
      tabWidth -= 1
      out.println(s"${addTab}}")
      tabWidth -= 1
      out.println(s"${addTab}}")
    }
    out.close()
    sout.toString
  }

    
  override def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.print(indent)
    super.emitVarDef(sym, rhs)
  }
  
  override def emitAssignment(lhs: String, rhs: String): Unit = {
    stream.print(indent)
    super.emitAssignment(lhs, rhs)
  }

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.print(indent)
    super.emitValDef(sym, rhs)
  }

  private def freshMut[T:Manifest](name: String) = {
    val sym = reflectMutableSym(fresh[T])
    namedSyms.put(sym, name)
    sym
  }

}

object MakeOperators {
  def main(args: Array[String]) = {
    val xIR = new BaseExp with ExtraBaseExp with DenseVectorBuilderOps with NumericOpsExp with OrderingOpsExp with RangeOpsExp with DenseVectorOpsExp with trochee.basic.SimpleFieldsExp with LiftVariables with SparseVectorBuilderOps with SparseVectorOpsExp with HashVectorBuilderOps with HashVectorOpsExp with IfThenElseExp with WhileExp with PrimitiveOpsExp with ExtraNumericOpsExp with BitSetOpsExp with DVSVBuilderOps
    val gen = new GenOperators with ScalaSimpleFieldsGen with ScalaGenRangeOps with ScalaGenNumericOps  with ScalaGenVariables with ScalaGenOrderingOps with ScalaGenWhile with ScalaGenBooleanOps with ScalaGenIfThenElse with ScalaGenPrimitiveOps with ScalaExtraNumericGen with ScalaBitSetGen {
      val IR: xIR.type = xIR 
    }
    import xIR._

    for(op <- numericalOps[Double]) {
      println(gen.genBinaryOp(vectorBinaryOp(denseVectorHelper[Double, Double, Double], op)))
      println(gen.genBinaryOp(vectorBinaryOp(denseVectorScalarHelper[Double, Double, Double], op)))
      println(gen.genBinaryUpdateOp(vectorBinaryUpdateOp(denseVectorTransformer[Double, Double], op)))
      println(gen.genBinaryUpdateOp(vectorBinaryUpdateOp(denseVectorScalarTransformer[Double, Double], op)))
    }


    println(gen.genBinaryOp(vectorBinaryOp(denseVectorDotProductHelper[Double, Double, Double], opMulInner[Double])))
    println(gen.genBinaryOp(vectorBinaryOp(sparseVectorDotProductHelper[Double, Double, Double], opMulInner[Double])))

    for(op <- orderingOps[Double]) {
      println(gen.genBinaryOp(vectorBinaryOp(denseVectorHelper[Double, Double, Boolean], op)))
      println(gen.genBinaryOp(vectorBinaryOp(denseVectorScalarHelper[Double, Double, Boolean], op)))
    }

    println("Sparse")

    for(op <- numericalOps[Double]) {
      println(gen.genBinaryOp(vectorBinaryOp(sparseVectorHelper[Double, Double, Double], op)))
      println(gen.genBinaryOp(vectorBinaryOp(sparseVectorScalarHelper[Double, Double, Double], op)))
      println(gen.genBinaryUpdateOp(vectorBinaryUpdateOp(sparseVectorTransformer[Double, Double], op)))
      println(gen.genBinaryUpdateOp(vectorBinaryUpdateOp(sparseVectorScalarTransformer[Double, Double], op)))
    }

    for(op <- orderingOps[Double]) {
      println(gen.genBinaryOp(vectorBinaryOp(sparseVectorHelper[Double, Double, Boolean], op)))
      println(gen.genBinaryOp(vectorBinaryOp(sparseVectorScalarHelper[Double, Double, Boolean], op)))
    }

    println("Hash")

    for(op <- numericalOps[Double]) {
      println(gen.genBinaryOp(vectorBinaryOp(hashVectorHelper[Double, Double, Double], op)))
      println(gen.genBinaryOp(vectorBinaryOp(hashVectorScalarHelper[Double, Double, Double], op)))
      println(gen.genBinaryUpdateOp(vectorBinaryUpdateOp(hashVectorTransformer[Double, Double], op)))
      println(gen.genBinaryUpdateOp(vectorBinaryUpdateOp(hashVectorScalarTransformer[Double, Double], op)))
    }

    for(op <- orderingOps[Double]) {
      println(gen.genBinaryOp(vectorBinaryOp(hashVectorHelper[Double, Double, Boolean], op)))
      println(gen.genBinaryOp(vectorBinaryOp(hashVectorScalarHelper[Double, Double, Boolean], op)))
    }

    println(gen.genBinaryOp(vectorBinaryOp(hashVectorDotProductHelper[Double, Double, Double], opMulInner[Double])))

    println("DV/SV")

    
    for(op <- numericalOps[Double]) {
      if (op.preferIntersected) {
        val helper = dv_sv_sv_helper[Double, Double, Double]
        println(gen.genBinaryOp(vectorBinaryOp(helper, op)))
      } else {
        val helper = dv_sv_dv_helper[Double, Double, Double]
        println(gen.genBinaryOp(vectorBinaryOp(helper, op)))
      }
      println(gen.genBinaryUpdateOp(vectorBinaryUpdateOp(dv_sv_transformer[Double, Double], op)))
    }

  }

}
