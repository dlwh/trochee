package trochee.kernels

import scala.reflect.runtime.universe._
import virtualization.lms.common._
import virtualization.lms.internal.{FatExpressions, Effects, Expressions}
import reflect.{SourceContext, ClassTag}
import trochee.codegen.{OpenCLKernelGenArrayOps, OpenCLKernelGenNumericOps, OpenCLKernelCodegen}
import trochee.util.CStructExp
import trochee.basic.{SpireOpsExp, RingOpsExp, RingOps, RigOps}
import spire.algebra._
import spire.implicits._
import spire.syntax._
import spire.math._
import trochee.parser.{ExtraBase, ExtraBaseExp}

/**
 * 
 * @author dlwh
 */
trait KernelOps extends ExtraBase with RingOps with OrderingOps with IfThenElse { this: Base =>
//  type _Global[+T] = Rep[T with Global]
//  type Private[+T] = Rep[T with trochee.kernels.Private]
//  type Local[+T] = Rep[T with trochee.kernels.Local]
//  type Constant[+T] = Rep[T with trochee.kernels.Constant]
  def workDim(implicit pos: SourceContext): Rep[Int]
  def globalSize(dim: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def globalId(dim: Rep[Int])(implicit pos: SourceContext):Rep[Int]

//  def localSize(dim: Rep[Int]):Rep[Int]
//  def localId(dim: Rep[Int]): Rep[Int]
//  def numGroups(dim: Rep[Int]):Rep[Int]
//  def groupId(dim: Rep[Int]): Rep[Int]

//  def memFence(): Rep[Unit]

//  def infinity: Rep[Float]

//  def mad(a: Rep[Float], b: Rep[Float], c: Rep[Float]): Rep[Float]

  def kernel[T1:Manifest:TypeTag](name: String)(fn: (Rep[T1]) => Unit): Kernel
  def kernel2[T1:Manifest:TypeTag, T2: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2]) => Unit): Kernel
  def kernel3[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3]) => Unit): Kernel
  def kernel4[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4]) => Unit): Kernel
  def kernel5[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5]) => Unit): Kernel
  def kernel6[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag, T6: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5], Rep[T6]) => Unit): Kernel
  def kernel7[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag, T6: Manifest: TypeTag, T7: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5], Rep[T6], Rep[T7]) => Unit): Kernel
  def kernel8[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag, T6: Manifest: TypeTag, T7: Manifest: TypeTag, T8: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5], Rep[T6], Rep[T7], Rep[T8]) => Unit): Kernel
//  def kernel9[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag, T6: Manifest: TypeTag, T7: Manifest: TypeTag, T8: Manifest: TypeTag, T9: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5], Rep[T6], Rep[T7], Rep[T8], Rep[T9]) => Unit): Kernel

  type Kernel


  // nice implicits
  implicit def const(x: Int):Rep[Int] = unit(x)
  implicit def const(x: Float):Rep[Float] = unit(x)
  implicit def const(x: Long):Rep[Long] = unit(x)

}


abstract class AddVectorsKernel { this: Base with KernelOps =>
  def prog = kernel2("addVectors") { (x: Rep[Array[Double] with Global], y: Rep[Array[Double] with Global]) =>
    val id = globalId(0)
    x(id) = x(id) + y(id)
  }
}


trait KernelOpsExp extends KernelOps with BaseFatExp with VariablesExp with CStructExp with ExtraBaseExp with SpireOpsExp with OrderingOpsExp with IfThenElseExp with FunctionsExp with RingOpsExp {
  def workDim(implicit pos: SourceContext) : Rep[Int] = WorkDim()
  def globalSize(dim: Rep[Int])(implicit pos: SourceContext):Rep[Int] = GlobalSize(dim)(pos)
  def globalId(dim: Rep[Int])(implicit pos: SourceContext):Rep[Int] = GlobalId(dim)(pos)

  case class Ignore[T]() extends Def[T]
  case class WorkDim() extends Def[Int]
  case class GlobalSize(dim: Rep[Int])(implicit val pos: SourceContext) extends Def[Int]
  case class GlobalId(dim: Rep[Int])(implicit val pos: SourceContext) extends Def[Int]

  case class Kernel protected[KernelOpsExp] (name: String, args: List[Sym[_]], fn: Block[Unit], qualifiers: IndexedSeq[String]) {
    require(args.length == this.qualifiers.length)
  }

  override def kernel[T1:Manifest:TypeTag](name: String)(fn: (Rep[T1]) => Unit): Kernel = {
    val x1 = freshMut[T1]
    val y = reifyEffects(unit(fn(x1))) // unfold completely at the definition site.
    Kernel(name, List(x1), y, IndexedSeq(qualifier[T1]))
  }

  override def kernel2[T1:Manifest:TypeTag, T2: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2]) => Unit): Kernel = {
    val x1 = freshMut[T1]
    val x2 = freshMut[T2]
    val y = reifyEffects(unit(fn(x1, x2))) // unfold completely at the definition site.
    Kernel(name, List(x1, x2), y, IndexedSeq(qualifier[T1], qualifier[T2]))
  }

  def freshMut[T:Manifest] = reflectMutableSym(fresh[T])

  override def kernel3[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3]) => Unit): Kernel = {
    val x1 = freshMut[T1]
    val x2 = freshMut[T2]
    val x3 = freshMut[T3]
    val y = reifyEffects(unit(fn(x1, x2, x3))) // unfold completely at the definition site.
    Kernel(name, List(x1, x2, x3), y, IndexedSeq(qualifier[T1], qualifier[T2], qualifier[T3]))
  }

  override def kernel4[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4]) => Unit): Kernel = {
    val x1 = freshMut[T1]
    val x2 = freshMut[T2]
    val x3 = freshMut[T3]
    val x4 = freshMut[T4]
    val y = reifyEffects(unit(fn(x1, x2, x3, x4))) // unfold completely at the definition site.
    Kernel(name, List(x1, x2, x3, x4), y, IndexedSeq(qualifier[T1], qualifier[T2], qualifier[T3], qualifier[T4]))
  }

  override def kernel5[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5]) => Unit): Kernel = {
    val x1 = freshMut[T1]
    val x2 = freshMut[T2]
    val x3 = freshMut[T3]
    val x4 = freshMut[T4]
    val x5 = freshMut[T5]
    val y = reifyEffects(unit(fn(x1, x2, x3, x4, x5))) // unfold completely at the definition site.
    Kernel(name, List(x1, x2, x3, x4, x5), y, IndexedSeq(qualifier[T1], qualifier[T2], qualifier[T3], qualifier[T4], qualifier[T5]))
  }

  override def kernel6[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag, T6: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5], Rep[T6]) => Unit): Kernel = {
    val x1 = freshMut[T1]
    val x2 = freshMut[T2]
    val x3 = freshMut[T3]
    val x4 = freshMut[T4]
    val x5 = freshMut[T5]
    val x6 = freshMut[T6]
    val y = reifyEffects(unit(fn(x1, x2, x3, x4, x5, x6))) // unfold completely at the definition site.
    Kernel(name, List(x1, x2, x3, x4, x5, x6), y, IndexedSeq(qualifier[T1], qualifier[T2], qualifier[T3], qualifier[T4], qualifier[T5], qualifier[T6]))
  }

  override def kernel7[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag, T6: Manifest: TypeTag, T7: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5], Rep[T6], Rep[T7]) => Unit): Kernel = {
    val x1 = freshMut[T1]
    val x2 = freshMut[T2]
    val x3 = freshMut[T3]
    val x4 = freshMut[T4]
    val x5 = freshMut[T5]
    val x6 = freshMut[T6]
    val x7 = freshMut[T7]
    val y = reifyEffects(unit(fn(x1, x2, x3, x4, x5, x6, x7))) // unfold completely at the definition site.
    Kernel(name, List(x1, x2, x3, x4, x5, x6, x7), y, IndexedSeq(qualifier[T1], qualifier[T2], qualifier[T3], qualifier[T4], qualifier[T5], qualifier[T6], qualifier[T7]))
  }

  override def kernel8[T1:Manifest:TypeTag, T2: Manifest: TypeTag, T3: Manifest: TypeTag, T4: Manifest: TypeTag, T5: Manifest: TypeTag, T6: Manifest: TypeTag, T7: Manifest: TypeTag, T8: Manifest: TypeTag](name: String)(fn: (Rep[T1], Rep[T2], Rep[T3], Rep[T4], Rep[T5], Rep[T6], Rep[T7], Rep[T8]) => Unit): Kernel = {
    val x1 = freshMut[T1]
    val x2 = freshMut[T2]
    val x3 = freshMut[T3]
    val x4 = freshMut[T4]
    val x5 = freshMut[T5]
    val x6 = freshMut[T6]
    val x7 = freshMut[T7]
    val x8 = freshMut[T8]
    val y = reifyEffects(unit(fn(x1, x2, x3, x4, x5, x6, x7, x8))) // unfold completely at the definition site.
    Kernel(name, List(x1, x2, x3, x4, x5, x6, x7, x8), y, IndexedSeq(qualifier[T1], qualifier[T2], qualifier[T3], qualifier[T4], qualifier[T5], qualifier[T6], qualifier[T7], qualifier[T8]))
  }




  // TODO UGH
  protected def qualifier[T: TypeTag]:String = {
    if(typeTag[T].tpe.baseClasses.exists(_.toString == "class Global")) "__global"
    else if(typeTag[T].tpe.baseClasses.exists(_.toString == "class Local")) "__local"
    else if(typeTag[T].tpe.baseClasses.exists(_.toString == "class Private")) "__private"
    else if(typeTag[T].tpe.baseClasses.exists(_.toString == "class Constant")) "__constant"
    else typeTag[T].tpe match {
      case AnnotatedType(anns, _, _) =>
        if(anns.exists(_.toString == "trochee.kernels.Global")) "__global"
        else if(anns.exists( (_:Annotation).tpe <:< typeTag[trochee.kernels.Local].tpe)) "__local"
        else if(anns.exists( (_: Annotation).tpe <:< typeTag[trochee.kernels.Private].tpe)) "__private"
        else if(anns.exists( (_: Annotation).tpe <:< typeTag[trochee.kernels.Constant].tpe)) "__constant"
        else ""
      case _ => ""
    }
  }


}



/*
trait ScalaGenKernel extends ScalaGenBase with ScalaGenArrayOps with ScalaGenNumericOps {
  val IR: Expressions with ArrayOpsExp with NumericOpsExp with KernelOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case _ => super.emitNode(sym, node)
  }

}
*/


/*

Scala-specific kernel generation strategy:
def addVectors(x: Array[Double], y: Array[Double])(implicit context: GroupContext) = {
   val id = globalId(0) // context used as implicit, which is just context.global(0)
   x(id) += y(id)
}


scheduling:
def run(a: Array[Double], b: Array[Double], globalWorkSize: Array[Int]) = {
def rec(offset: Int): Array[Array[Int]] = {
   if(offset == globalWorkSize.length) Array.empty
   else {
     val arr = rec(offset+1)
     for(x <- 0 until globalWorkSize(offset); z <- arr) yield x :+ z)
   }

  rec(offset).par(addVectors(x,y)(Context(x,y,z)))
*/



object Usage {
  val concreteProg = new AddVectorsKernel with EffectExp with KernelOpsExp { self =>
    val IR: self.type = self
    val codegen = new OpenCLKernelCodegen with OpenCLKernelGenArrayOps with OpenCLKernelGenNumericOps {
      val IR: self.type = self
    }
    println(codegen.mkKernel(prog))
  }

}