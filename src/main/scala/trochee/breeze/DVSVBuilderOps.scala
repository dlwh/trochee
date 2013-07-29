package trochee.breeze

import scala.virtualization.lms.common.{While, IfThenElse, OrderingOps, Base}
import breeze.math.Semiring
import breeze.linalg.{SparseVector, DenseVector}
import trochee.basic.ExtraBase

/**
 * TODO
 *
 * @author dlwh
 **/
trait DVSVBuilderOps extends OpGeneratorOps with DenseVectorOps with SparseVectorOps { this: ExtraBase with While =>

  def dv_sv_dv_helper[L: Manifest, R: Manifest, Res: Manifest:Semiring] =  new VectorOpHelper[DenseVector[L], L, SparseVector[R], R, DenseVector[Res], Res] {
    def intersected(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = {
      throw new UnsupportedOperationException("Use DV_SV_SV for intersected-type operations")
    }


    def fullRange(lhs: Rep[DenseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, DenseVector[Res], Res] = new VectorBuilder[L, R, DenseVector[Res], Res] {
      def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[DenseVector[Res]] = {
        val res = newDenseVector[Res](lhs.length)
        val resdata = res.data
        val rdata = rhs.data
        val rindex = rhs.index
        val rsize = rhs.activeSize
        var rpos = unit(0)
        while(rpos < rsize) {
          val i = rindex(rpos)
          resdata(i) = f(lhs(i), rdata(rpos))
          rpos += 1
        }

        res
      }
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
          var rpos = unit(0)
          while(rpos < rsize) {
            val i = rindex(rpos)
            builder.add(i, f(lhs(i), rdata(rpos)))
            rpos += 1
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
}
