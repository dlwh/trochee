package trochee.breeze

import trochee.basic.ExtraBase
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import breeze.linalg.DenseVector
import breeze.math.Semiring
import breeze.linalg.operators._
import scala.reflect.runtime._
import universe._


/**
 * TODO
 *
 * @author dlwh
 **/
trait OpGeneratorOps { this: Base with ExtraBase with NumericOps =>

  case class Operator[LHS, RHS, Result](op: OpType,
                                        zeroIsNilpotent: Boolean = false,
                                        zeroIsIdempotent: Boolean = false,
                                        rhsZeroIsIdempotent: Boolean = false,
                                        lhsZeroIsNilpotent: Boolean = true)
                                       (val fn: (Rep[LHS],Rep[RHS])=>Rep[Result]) {
    def apply(lhs: Rep[LHS], rhs: Rep[RHS]):Rep[Result] = fn(lhs, rhs)
  }

  trait VectorOpHelper[LHS, LHSV,
                       RHS, RHSV,
                       Result, ResultV] {
    def intersected(lhs: Rep[LHS], rhs: Rep[RHS]):VectorBuilder[LHSV, RHSV, Result, ResultV]
    def union(lhs: Rep[LHS], rhs: Rep[RHS]): VectorBuilder[LHSV, RHSV, Result, ResultV]
    def fullRange(lhs: Rep[LHS], rhs: Rep[RHS]): VectorBuilder[LHSV, RHSV, Result, ResultV]

  }

  trait VectorTransformHelper[LHS, LHSV, RHS, RHSV] {
    def intersected(lhs: Rep[LHS], rhs: Rep[RHS]):VectorUpdater[LHSV, RHSV]
    def union(lhs: Rep[LHS], rhs: Rep[RHS]): VectorUpdater[LHSV, RHSV]
    def fullRange(lhs: Rep[LHS], rhs: Rep[RHS]): VectorUpdater[LHSV, RHSV]
  }

  trait VectorBuilder[LHS, RHS, Result, ResultV] {
    def map(f: (Rep[LHS], Rep[RHS]) => Rep[ResultV]):Rep[Result]
  }

  trait VectorUpdater[LHS, RHS] {
    def updateLHS(f: (Rep[LHS], Rep[RHS]) => Rep[LHS]):Rep[Unit]
  }

  case class VectorBinaryOperator[LHS, RHS, Result](lhs: Type, op: OpType, rhs: Type, result: Type)(val body: (Rep[LHS],Rep[RHS])=>Rep[Result]) {
    def generatedName = s"${mkAbbreviation(lhs)}_${mkAbbreviation(rhs)}_${op.getClass.getSimpleName.replaceAll("[$]","")}_${mkAbbreviation(result)}"
  }

  case class VectorBinaryUpdateOperator[LHS, RHS](lhs: Type, op: OpType, rhs: Type)(val body: (Rep[LHS],Rep[RHS])=>Rep[Unit]) {
    def generatedName = s"${mkAbbreviation(lhs)}_${mkAbbreviation(rhs)}_${op.getClass.getSimpleName.replaceAll("[$]","")}Into"
  }

  def mkAbbreviation[T](implicit tag: Type):String = {
    tag match {
      case TypeRef(_, name, args) =>
        val prefix = name.toString.filter(c => c.isLetter && c.isUpper)
        if(args.isEmpty) prefix
        else args.map(_.toString.filter(_.isUpper)).mkString(prefix +"_", "_", "_")
    }
  }

  def vectorBinaryOp[LHS:TypeTag, LHSV:TypeTag,
                     RHS:TypeTag, RHSV:TypeTag,
                     Result:TypeTag, ResultV:TypeTag](helper: VectorOpHelper[LHS, LHSV, RHS, RHSV, Result, ResultV],
                                      op: Operator[LHSV, RHSV, ResultV]):VectorBinaryOperator[LHS, RHS, Result] =  {

    val body = { (lhs: Rep[LHS], rhs: Rep[RHS]) =>
      val zipped = {
        if(op.zeroIsNilpotent) helper.intersected(lhs, rhs)
        else if(op.zeroIsIdempotent) helper.union(lhs, rhs)
        else helper.fullRange(lhs, rhs)
      }
      zipped map {op(_, _)}
    }


    VectorBinaryOperator(typeOf[LHS], op.op, typeOf[RHS], typeOf[Result])(body)
  }

  def vectorBinaryUpdateOp[LHS:TypeTag, LHSV:TypeTag, RHS:TypeTag, RHSV:TypeTag](helper: VectorTransformHelper[LHS, LHSV, RHS, RHSV],
                                   op: Operator[LHSV, RHSV, LHSV]):VectorBinaryUpdateOperator[LHS, RHS] =  {

    val body = { (lhs: Rep[LHS], rhs: Rep[RHS]) =>
      val zipped = {
        if(op.zeroIsNilpotent) helper.intersected(lhs, rhs)
        else if(op.zeroIsIdempotent) helper.union(lhs, rhs)
        else helper.fullRange(lhs, rhs)
      }
      zipped updateLHS  {op(_, _)}
    }


    VectorBinaryUpdateOperator(typeOf[LHS], op.op, typeOf[RHS])(body)
  }

  def opAdd[T:Manifest:Numeric] = Operator[T, T, T](OpAdd, zeroIsIdempotent = true)({_ + _})
  def opSub[T:Manifest:Numeric] = Operator[T, T, T](OpSub, rhsZeroIsIdempotent = true)({_ - _})
  def opMulScalar[T:Manifest:Numeric] = Operator[T, T, T](OpMulScalar, zeroIsNilpotent = true)({_ * _})
  def opDiv[T:Manifest:Numeric] = Operator[T, T, T](OpDiv, lhsZeroIsNilpotent = true)({_ / _})
  //def opMod[T:Manifest:Numeric] = Operator[T, T, T]("OpMod", lhsZeroIsNilpotent = true)({_ % _})


}

trait DenseVectorBuilderOps extends OpGeneratorOps with DenseVectorOps { this: Base with ExtraBase with NumericOps with RangeOps with Variables with Effects with LiftVariables =>

  def denseVectorHelper[L:Manifest, R:Manifest, Res:Manifest:Semiring] = new VectorOpHelper[DenseVector[L], L, DenseVector[R], R, DenseVector[Res], Res] {

    def fullRange(lhs: Rep[DenseVector[L]], rhs: Rep[DenseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = {
      new VectorBuilder[L, R, DenseVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[DenseVector[Res]] = {
          val dv: Rep[DenseVector[Res]] = repDenseVector.zeros[Res](lhs.length)
          val dataRes = dv.data
          val ldata = lhs.data
          val rdata = rhs.data
          var loff = lhs.offset
          var roff = rhs.offset
          val lstride = lhs.stride
          val rstride = rhs.stride
          for(i <- unit(0) until dv.length) {
            dataRes(i) = f(ldata(loff), rdata(roff))
            loff = loff + lstride
            roff = roff + rstride
          }
          dv
        }
      }
    }

    def union(lhs: Rep[DenseVector[L]], rhs: Rep[DenseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[DenseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = fullRange(lhs, rhs)
  }

  def denseVectorScalarHelper[L:Manifest, R:Manifest, Res:Manifest:Semiring] = new VectorOpHelper[DenseVector[L], L, R, R, DenseVector[Res], Res] {

    def fullRange(lhs: Rep[DenseVector[L]], rhs: Rep[R]): VectorBuilder[L, R, DenseVector[Res], Res] = {
      new VectorBuilder[L, R, DenseVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[DenseVector[Res]] = {
          val dv: Rep[DenseVector[Res]] = repDenseVector.zeros[Res](lhs.length)
          val dataRes = dv.data
          val ldata = lhs.data
          var loff = lhs.offset
          val lstride = lhs.stride
          for(i <- unit(0) until dv.length) {
            dataRes(i) = f(ldata(loff), rhs)
            loff = loff + lstride
          }
          dv
        }
      }
    }

    def union(lhs: Rep[DenseVector[L]], rhs: Rep[R]): VectorBuilder[L, R, DenseVector[Res], Res] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[R]): VectorBuilder[L, R, DenseVector[Res], Res] = fullRange(lhs, rhs)
  }


  def denseVectorTransformer[L:Manifest, R:Manifest] = new VectorTransformHelper[DenseVector[L], L, DenseVector[R], R] {

    def fullRange(lhs: Rep[DenseVector[L]], rhs: Rep[DenseVector[R]]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
          val ldata = lhs.data
          val rdata = rhs.data
          var loff = lhs.offset
          var roff = rhs.offset
          val lstride = lhs.stride
          val rstride = rhs.stride
          for(i <- unit(0) until lhs.length) {
            ldata(loff) = f(ldata(loff), rdata(roff))
            loff = loff + lstride
            roff = roff + rstride
          }
        }
      }
    }

    def union(lhs: Rep[DenseVector[L]], rhs: Rep[DenseVector[R]]): VectorUpdater[L, R] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[DenseVector[R]]): VectorUpdater[L, R] = fullRange(lhs, rhs)
  }

  def denseVectorScalarTransformer[L:Manifest, R:Manifest] = new VectorTransformHelper[DenseVector[L], L, R, R] {

    def fullRange(lhs: Rep[DenseVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]) = {
          val ldata = lhs.data
          var loff = lhs.offset
          val lstride = lhs.stride
          for(i <- unit(0) until lhs.length) {
            ldata(loff) = f(ldata(loff), rhs)
            loff = loff + lstride
          }
        }
      }
    }

    def union(lhs: Rep[DenseVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = fullRange(lhs, rhs)
  }



}

