package trochee.breeze

import trochee.basic.{ExtraNumericOps, ExtraBase}
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
trait OpGeneratorOps { this: Base with ExtraBase with NumericOps with OrderingOps with ExtraNumericOps =>

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

  def orderingOps[T:Manifest:Ordering] = IndexedSeq(opLT, opLTE, opGT, opGTE, opEq, opNe)
  def numericalOps[T:Manifest:Numeric] = IndexedSeq(opAdd, opSub, opMulScalar, opDiv, opMod)

  def opAdd[T:Manifest:Numeric] = Operator[T, T, T](OpAdd, zeroIsIdempotent = true)({_ + _})
  def opSub[T:Manifest:Numeric] = Operator[T, T, T](OpSub, rhsZeroIsIdempotent = true)({_ - _})
  def opMulScalar[T:Manifest:Numeric] = Operator[T, T, T](OpMulScalar, zeroIsNilpotent = true)({_ * _})
  def opDiv[T:Manifest:Numeric] = Operator[T, T, T](OpDiv, lhsZeroIsNilpotent = true)({_ / _})
  def opMod[T:Manifest:Numeric] = Operator[T, T, T](OpMod, lhsZeroIsNilpotent = true)({_ % _})

  def opLT[T:Manifest:Ordering] = Operator[T, T, Boolean](OpLT)({_ < _})
  def opLTE[T:Manifest:Ordering] = Operator[T, T, Boolean](OpLTE)({_ <= _})
  def opGT[T:Manifest:Ordering] = Operator[T, T, Boolean](OpGT)({_ > _})
  def opGTE[T:Manifest:Ordering] = Operator[T, T, Boolean](OpGTE)({_ >= _})
  def opEq[T:Manifest:Ordering] = Operator[T, T, Boolean](OpEq)({_ === _})
  def opNe[T:Manifest:Ordering] = Operator[T, T, Boolean](OpNe)({_ !== _})

  protected def mkAbbreviation[T](implicit tag: Type):String = {
    tag match {
      case TypeRef(_, name, args) =>
        val prefix = name.toString.filter(c => c.isLetter && c.isUpper)
        if(args.isEmpty) prefix
        else args.map(_.toString.filter(_.isUpper)).mkString(prefix +"_", "_", "_")
    }
  }
}


