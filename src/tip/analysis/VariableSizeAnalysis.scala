package tip.analysis

import tip.ast.AstNodeData.DeclarationData
import tip.cfg.{CfgNode, IntraproceduralProgramCfg}
import tip.lattices.IntervalLattice
import tip.solvers.{WorklistFixpointSolverWithReachabilityAndWidening, WorklistFixpointSolverWithReachabilityAndWideningAndNarrowing}

trait VariableSizeAnalysisWidening extends IntervalAnalysisWidening {

  override protected val B: Set[IntervalLattice.Num] = Set(
    0, 1,
    Byte.MinValue, Byte.MaxValue,
    Char.MinValue, Char.MaxValue,
    Int.MinValue, Int.MaxValue,
    IntervalLattice.MInf, IntervalLattice.PInf
  )
}

object VariableSizeAnalysis {

  object Intraprocedural {

    /**
     * Interval analysis, using the worklist solver with init and widening.
     */
    class WorklistSolverWithWidening(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
      extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWidening[CfgNode]
        with VariableSizeAnalysisWidening

    /**
     * Interval analysis, using the worklist solver with init, widening, and narrowing.
     */
    class WorklistSolverWithWideningAndNarrowing(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
      extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWideningAndNarrowing[CfgNode]
        with VariableSizeAnalysisWidening {

      val narrowingSteps = 5
    }
  }
}

