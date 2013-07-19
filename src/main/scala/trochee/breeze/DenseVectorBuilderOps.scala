package trochee.breeze

import breeze.linalg.DenseVector
import scala.virtualization.lms.common._
import trochee.basic._
import breeze.math.Semiring
import scala.virtualization.lms.internal.Effects

/**
 * TODO
 *
 * @author dlwh
 **/
trait DenseVectorBuilderOps extends OpGeneratorOps with DenseVectorOps { this: Base with ExtraBase with NumericOps with OrderingOps with RangeOps with Variables with Effects with LiftVariables with ExtraNumericOps =>

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
