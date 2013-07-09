package trochee.breeze

import trochee.basic.ExtraBase
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import breeze.linalg.DenseVector
import breeze.math.Semiring
import scala.reflect.SourceContext


/**
 * TODO
 *
 * @author dlwh
 **/
trait OpGeneratorOps { this: Base with ExtraBase with NumericOps =>

  case class Operator[LHS, RHS, Result](name: String,
                                        zeroIsNilpotent: Boolean = false,
                                        zeroIsIdempotent: Boolean = false,
                                        rhsZeroIsIdempotent: Boolean = false,
                                        lhsZeroIsNilpotent: Boolean = true)
                                       (val fn: (Rep[LHS],Rep[RHS])=>Rep[Result]) {
    def apply(lhs: Rep[LHS], rhs: Rep[RHS]):Rep[Result] = fn(lhs, rhs)
  }

  trait VectorOpHelper[LHS[_], LHSV,
                       RHS[_], RHSV,
                       Result, ResultV] {
    def name: String
    def intersected(lhs: Rep[LHS[LHSV]], rhs: Rep[RHS[RHSV]]):VectorBuilder[LHSV, RHSV, Result, ResultV]
    def union(lhs: Rep[LHS[LHSV]], rhs: Rep[RHS[RHSV]]): VectorBuilder[LHSV, RHSV, Result, ResultV]
    def fullRange(lhs: Rep[LHS[LHSV]], rhs: Rep[RHS[RHSV]]): VectorBuilder[LHSV, RHSV, Result, ResultV]
  }

  trait VectorBuilder[LHS, RHS, Result, ResultV] {
    def map(f: (Rep[LHS], Rep[RHS]) => Rep[ResultV]):Rep[Result]
  }

  def vectorBinaryOp[LHS[_], LHSV,
                     RHS[_], RHSV,
                     Result, ResultV](helper: VectorOpHelper[LHS, LHSV, RHS, RHSV, Result, ResultV],
                                         op: Operator[LHSV, RHSV, ResultV]) = { (lhs: Rep[LHS[LHSV]], rhs: Rep[RHS[RHSV]]) =>
      val zipped = {
        if(op.zeroIsNilpotent) helper.intersected(lhs, rhs)
        else if(op.zeroIsIdempotent) helper.union(lhs, rhs)
        else helper.fullRange(lhs, rhs)
      }
      zipped map {op(_, _)}
  }

  def opAdd[T:Manifest:Numeric] = Operator[T, T, T]("OpAdd", zeroIsIdempotent = true)({_ + _})
  def opSub[T:Manifest:Numeric] = Operator[T, T, T]("OpSub", rhsZeroIsIdempotent = true)({_ - _})
  def opMulScalar[T:Manifest:Numeric] = Operator[T, T, T]("OpMulScalar", zeroIsNilpotent = true)({_ * _})
  def opDiv[T:Manifest:Numeric] = Operator[T, T, T]("OpDiv", lhsZeroIsNilpotent = true)({_ / _})
  //def opMod[T:Manifest:Numeric] = Operator[T, T, T]("OpMod", lhsZeroIsNilpotent = true)({_ % _})


}

trait DenseVectorBuilderOps extends OpGeneratorOps with DenseVectorOps { this: Base with ExtraBase with NumericOps with RangeOps with Variables with Effects with LiftVariables =>

  def denseVectorHelper[L:Manifest, R:Manifest, Res:Manifest:Semiring] = new VectorOpHelper[DenseVector, L, DenseVector, R, DenseVector[Res], Res] {
    def name: String = "DV_DV_DV"

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
          return dv
        }
      }
    }

    def union(lhs: Rep[DenseVector[L]], rhs: Rep[DenseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[DenseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = ???
  }




}

