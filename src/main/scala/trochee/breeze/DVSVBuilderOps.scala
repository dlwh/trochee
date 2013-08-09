package trochee.breeze

import scala.virtualization.lms.common._
import breeze.math.Semiring
import breeze.linalg.{SparseVector, DenseVector}
import trochee.basic.ExtraBase

/**
 * TODO
 *
 * @author dlwh
 **/
trait DVSVBuilderOps extends OpGeneratorOps with DenseVectorOps with SparseVectorOps with RangeOps { this: ExtraBase with While =>

  def dv_sv_dv_helper[L: Manifest, R: Manifest, Res: Manifest:Semiring] =  new VectorOpHelper[DenseVector[L], L, SparseVector[R], R, DenseVector[Res], Res] {
    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = {
      throw new UnsupportedOperationException("Use DV_SV_SV for intersected-type operations")
    }


    def fullRange(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = new VectorBuilder[L, R, DenseVector[Res], Res] {
      def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[DenseVector[Res]] = {
        val res = newDenseVector[Res](lhs.length)
        val resdata = res.data
        for (i <- unit(0) until res.length) {
          resdata(i) = f(lhs(i), rhs(i))
        }

        res
      }

      def zero: Rep[Int] = unit(0)
    }

    def union(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = fullRange(lhs, rhs)
  }


  def dv_sv_sv_helper[L: Manifest, R: Manifest, Res: Manifest:Semiring] =  new VectorOpHelper[DenseVector[L], L, SparseVector[R], R, SparseVector[Res], Res] {
    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, SparseVector[Res], Res] = {
      new VectorBuilder[L, R, SparseVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[SparseVector[Res]] = {
          val rsize = rhs.activeSize
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newSparseVectorBuilder(lhs.length, rsize)
          val rdata = rhs.data
          val rindex = rhs.index
          for (rpos <- unit(0) until rsize) {
            val i = rindex(rpos)
            builder.add(i, f(lhs(i), rdata(rpos)))
          }
          builder.toSparseVector(alreadySorted=unit(true), keysAlreadyUnique = unit(true))
        }
      }



    }

    def fullRange(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, SparseVector[Res], Res] = {
      throw new UnsupportedOperationException("Use DV_SV_DV for full-range-type operations")
    }

    def union(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, SparseVector[Res], Res] = {
      throw new UnsupportedOperationException("Use DV_SV_DV for union-type operations")
    }
  }


  def dv_sv_transformer[L: Manifest, R: Manifest] =  new VectorTransformHelper[DenseVector[L], L, SparseVector[R], R] {
    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorUpdater[L, R] = new VectorUpdater[L, R] {
      def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
        val rsize = rhs.activeSize
        val rdata = rhs.data
        val rindex = rhs.index
        for (rpos <- unit(0) until rsize) {
          val i = rindex(rpos)
          lhs(i) = f(lhs(i), rdata(rpos))
        }
      }
    }

    def union(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorUpdater[L, R] = fullRange(lhs, rhs)

    def fullRange(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
          for (i <- unit(0) until lhs.length) {
            lhs(i) = f(lhs(i), rhs(i))
          }
        }
      }
    }
  }
}
