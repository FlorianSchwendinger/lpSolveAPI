\name{lp.control.options}
\alias{lp.control.options}

\title{Solver Control Options}

\description{
A description of the various control options that can be set in an lpSolve linear program model object using the \code{\link{lp.control}} function.
}

\section{Control Options}{
  \describe{

    \item{anti.degen}{a character vector containing one or more of the following options. If any element is "none" then no anti-degeneracy handling is used.

      \tabular{rl}{
       \code{"none"}: \tab No anti-degeneracy handling. \cr
       \code{"fixedvars"}: \tab Check if there are equality slacks in the basis and try to drive them out in order to reduce chance of degeneracy in Phase 1. \cr
       \code{"columncheck"}: \tab \cr
       \code{"stalling"}: \tab \cr
       \code{"numfailure"}: \tab \cr
       \code{"lostfeas"}: \tab \cr
       \code{"infeasible"}: \tab \cr
       \code{"dynamic"}: \tab \cr
       \code{"duringbb"}: \tab \cr
       \code{"rhsperturb"}: \tab Perturbation of the working RHS at refactorization \cr
       \code{"boundflip"}: \tab Limit bound flips that can sometimes contribute to degeneracy in some models. \cr
      }

    The default is \code{c("infeasible", "stalling", "fixedvars")}.}

    \item{basis.crash}{a character string specifying the basis crash mode to use. When no basis crash is done the initial basis from which lpSolve starts is the basis containing all slack or artificial variables. When basis crash is enabled, a heuristic \emph{crash procedure} is executed before the first simplex iteration to quickly choose a basis matrix that has fewer artificial variables. This procedure tends to reduce the number of iterations required by the solver. The choices for this option are given in the following table.

      \tabular{rl}{
       \code{"none"}: \tab No basis crash. \cr
       \code{"mostfeasible"}: \tab Most feasible basis. \cr
       \code{"leastdegenerate"}: \tab Construct a basis that is in some sense the \emph{least} degenerate. \cr
      }

      The default is \code{"none"}.}

    \item{bb.depthlimit}{a single integer value specifying the maximum branch-and-bound depth. A positive value means that the depth limit is absolute. A negative value means a relative branch-and-bound depth limit. The \emph{order} of an MIP problem is defined to be 2 times the number of binary variables plus the number of semi-continuous (SC) and special-ordered-sets (SOS) variables. A relative value of \code{-x} results in a maximum depth of \code{x} times the order of the MIP problem.

      This control option only applies if there are integer, SC or SOS variables in the model (i.e., when the branch-and-bound algorithm is used). The branch-and-bound algorithm will not go deeper than this level. Limiting the depth speeds up the solving time but there is a chance that the solution obtained is sub-optimal. Be aware of this. Another possible consequence is that no solution will be found.

      The default value is \code{-50}; a value of zero implies no limit to the depth.}

    \item{bb.floorfirst}{a character string from among the following choices specifying which branch to take first in the branch-and-bound algorithm.

      \tabular{rl}{
       \code{"ceiling"}: \tab Take ceiling branch first. \cr
       \code{"floor"}: \tab Take floor branch first. \cr
       \code{"auto"}: \tab lpSolve decides which branch to take first. \cr
      }

      The value of this option can influence solving times considerably. However, the real-world performance will be model dependent. The default is \code{"auto"}.}

    \item{bb.rule}{a character vector specifying the branch-and-bound rule. The first element must be chosen from the following table.

      \tabular{rl}{
       \code{"first"}: \tab Select the lowest indexed non-integer column. \cr
       \code{"gap"}: \tab Selection based on the distance from the current bounds. \cr
       \code{"range"}: \tab Selection based on the largest current bound. \cr
       \code{"fraction"}: \tab Selection based on the largest fractional value. \cr
       \code{"pseudocost"}: \tab Simple, unweighted pseudo-cost of a variable. \cr
       \code{"pseudononint"}: \tab An extended pseudo-costing strategy based on minimizing the number of integer infeasibilities. \cr
       \code{"pseudoratio"}: \tab An extended pseudo-costing strategy based on maximizing the normal pseudo-cost divided by the number of infeasibilities. Effectively, it is similar to (the reciprocal of) a cost/benefit ratio. \cr
      }

      Additional modes (if any) may be appended to augment the rule specified in the first element of \code{bb.rule}.

      \tabular{rl}{
       \code{"weightreverse"}: \tab Select by criterion minimum (worst), rather than by criterion maximum (best). \cr
       \code{"branchreverse"}: \tab When \code{bb.floorfirst} is \code{"auto"}, select the  direction (lower/upper branch) opposite to that chosen by lpSolve. \cr
       \code{"greedy"}: \tab  \cr
       \code{"pseudocost"}: \tab Toggle between weighting based on pseudocost or objective function value. \cr
       \code{"depthfirst"}: \tab Select the node that has been selected before the most number of times. \cr
       \code{"randomize"}: \tab Add a randomization factor to the score for all the node candidates. \cr
       \code{"gub"}: \tab This option is still in development and should not be used at this time. \cr
       \code{"dynamic"}: \tab When \code{"depthfirst"} is selected, switch it off once the first solution is found. \cr
       \code{"restart"}: \tab Regularly restart the pseudocost value calculations. \cr
       \code{"breadthfirst"}: \tab Select the node that has been selected the fewest number of times (or not at all). \cr
       \code{"autoorder"}: \tab Create an \emph{optimal} branch-and-bound variable ordering. Can speed up branch-and-bound algorithm. \cr
       \code{"rcostfixing"}: \tab Do bound tightening during branch-and-bound based on the reduced cost information. \cr
       \code{"stronginit"}: \tab Initialize pseudo-costs by strong branching. \cr
      }

      The value of this rule can influence solving times considerably. However, the real-world performance will be model dependent. The default value is \code{c("pseudononint", "greedy", "dynamic", "rcostfixing")}.}

    \item{break.at.first}{a logical value. If \code{TRUE} then the branch-and-bound algorithm stops at the first solution found. The default (\code{FALSE}) is to continue until an optimal solution is found.}

    \item{break.at.value}{a numeric value. The branch-and-bound algorithm stops if the objective function becomes better than this value. The default (\eqn{\pm}infinity) is to continue until an optimal value is found.}

    \item{epslevel}{a character string providing a simplified way of specifying multiple tolerance thresholds in a \emph{logically} consistent way. The following values are set: \code{epsel}, \code{epsb}, \code{epsd}, \code{epspivot}, \code{epsint} and \code{mip.gap}.

      \tabular{rl}{
       \code{"tight"}: \tab Use tight tolerance values. \cr
       \code{"medium"}: \tab Use medium tolerance values. \cr
       \code{"loose"}: \tab Use loose tolerance values. \cr
       \code{"baggy"}: \tab Use very loose tolerance values. \cr
      }

    The default is \code{"tight"}.}

    \item{epsb}{a single positive numeric value specifying the tolerance used to determine whether a right-hand-side value should be considered zero. Rounding error in floating-point calculations may result in a loss of precision. A very small value (for example, 1e-99) could be the result of such errors and should be considered zero by the solver. If the absolute value of a right-hand-side value is less than \code{epsb} then it is treated as zero by the solver. The default value is \code{1.0e-10}.}

    \item{epsd}{a single positive numeric value specifying the tolerance used to determine whether a computed reduced cost should be considered zero. Rounding error in floating-point calculations may result in a loss of precision. A very small value (for example, 1e-99) could be the result of such errors and should be considered zero by the solver. If the absolute value of a computed reduced cost is less than \code{epsd} then it is treated as zero by the solver. The default value is \code{1.0e-9}.}

    \item{epsel}{a single positive numeric value specifying the tolerance used for rounding values to zero. Rounding error in floating-point calculations may result in a loss of precision. A very small value (for example, 1e-99) could be the result of such errors and should be considered zero by the solver. If the absolute value of a computed value is less than \code{epsel} then it is rounded to zero by the solver. The default value is \code{1.0e-12}. This parameter is used in situations where none of \code{epsint}, \code{epsb}, \code{epsd}, \code{epspivot} nor\code{epsperturb} apply.}

    \item{epsint}{a single positive numeric value specifying the tolerance used to determine whether a floating-point number is an integer. This parameter only applies when there is at least one integer variable so that the branch and bound algorithm is used. Integer variables are internally stored as floating-point. A tolerance is therefore needed to determine whether a value should be considered an integer. If the absolute value of the variable minus the closest integer is less than \code{epsint} then it is considered an integer. The default value is \code{1.0e-7}.}

    \item{epsperturb}{a single positive numeric value specifying the perturbation scalar for degenerate problems. The default is \code{1.0e-5}.}

    \item{epspivot}{a single positive numeric value specifying the tolerance used to determine whether a pivot element is zero. Rounding error in floating-point calculations may result in a loss of precision. A very small value (for example, 1e-99) could be the result of such errors and should be considered zero by the solver. If the absolute value of a computed pivot element is less than \code{epspivot} then it is treated as zero by the solver. Pivots will be performed on elements smaller (in absolute terms) than \code{epspivot} when no other larger pivot element can be found. The default value is \code{2.0e-7}.}

    \item{improve}{a character vector specifying the iterative improvement level. The possible values are given in the following table.

      \tabular{rl}{
        \code{"none"}: \tab None. \cr
        \code{"solution"}: \tab Running accuracy measurement of solved equations based on \eqn{Bx=r} (primal simplex), remedy is refactorization. \cr
        \code{"dualfeas"}: \tab Improve initial dual feasibility by bound flips (highly recommended). \cr
        \code{"thetagap"}: \tab Low-cost accuracy monitoring in the dual, remedy is refactorization. \cr
        \code{"bbsimplex"}: \tab By default there is a check for primal/dual feasibility at the optimum only for the relaxed problem, this also activates the test at the node level. \cr
      }

      The default is \code{c("dualfeas", "thetagap")}.}

    \item{infinite}{a positive numeric value specifying the practical value for infinity. This value is used for very large numbers, for example the upper bound of a variable without an upper bound. The default is \code{1.0e30}.}

    \item{maxpivot}{a positive integer value specifying the maximum number of pivots between re-inversion of the matrix. For stability, lpSolve periodically re-inverts the matrix. However, the more often this is done, the slower the solver becomes. The default is \code{250}.}

    \item{mip.gap}{a numeric vector of length two specifying respectively the absolute and relative MIP gaps used in the branch-and-bound algorithm. This tolerance is the difference between the best-found solution yet and the current solution. If the difference is smaller than this tolerance then the solution (and all the sub-solutions) is rejected. This can result in faster solving times, but results in a solution which is not the perfect solution. The default is \code{1.0e-11}.}

    \item{negrange}{a nonpositive numeric value below which variables are split into negative and positive parts. The default is \code{-1.0e6}.}

    \item{obj.in.bas}{a logical value specifying whether the objective function is stored in the matrix. The default is to store the objective function in the top row of the constraint matrix. If \code{FALSE} then the objective function is moved to separate storage. When the objective function is not stored in the basis the computation of reduced costs is somewhat slower. In the later versions of v5.5 there is the option to calculate reduced cost in the textbook way: completely independently of the basis.}

    \item{pivoting}{a character vector specifying the pivot rule (the rule for selecting row and column entering/leaving) and mode. The first element of this vector must be one of the four pivot rules listed in the first table. Remaining elements (if any) specify modes that modify this rule. The rule/mode can influence solving times considerably.

      \tabular{rl}{
        \code{"firstindex"}: \tab Select first. \cr
        \code{"dantzig"}: \tab Select according to Dantzig. \cr
        \code{"devex"}: \tab Devex pricing from Paula Harris. \cr
        \code{"steepestedge"}: \tab Steepest edge. \cr
      }

      \tabular{rl}{
        \code{"primalfallback"}: \tab When using the steepest edge rule, fall back to \code{"devex"} in the primal. \cr
        \code{"multiple"}: \tab A preliminary implementation of the multiple pricing scheme. Attractive candidate columns from one iteration may be used in subsequent iterations thus avoiding full updating of reduced costs. In the current implementation, lpSolve only reuses the second best entering column alternative. \cr
        \code{"partial"}: \tab Enables partial pricing. \cr
        \code{"adaptive"}: \tab Temporarily use an alternative strategy if cycling is detected. \cr
        \code{"randomize"}: \tab Adds a small randomization effect to the selected pricer. \cr
        \code{"autopartial"}: \tab Indicates automatic detection of segmented/staged/blocked models. It refers to partial pricing rather than full pricing. With full pricing, all non-basic columns are scanned, but with partial pricing only a subset is scanned for every iteration. This can speed up several models. \cr
        \code{"loopleft"}: \tab Scan entering/leaving columns left rather than right. \cr
        \code{"loopalternate"}: \tab Scan entering/leaving columns alternating left/right. \cr
        \code{"harristwopass"}: \tab Use Harris' primal pivot logic rather than the default. \cr
        \code{"truenorminit"}: \tab Use true norms for Devex and steepest edge initializations. \cr
      }
      The default is \code{c("devex", "adaptive")}.}

    \item{presolve}{a character vector specifying presolve steps to be carried out before solving. Presolve looks at the model and tries to simplify it so that solving times are shorter. For example a constraint on only one variable is converted to a bound on this variable (and the constraint is deleted) - the model dimensions can change because of this. Both rows and columns can be deleted by the presolve. Also, note that the \code{"lindep"} presolve option can result in the deletion of rows (the linear dependent ones). The \code{\link{get.constraints}} function will then return only the values of the rows that are kept.

  The presolve options are given in the following table. If any element of \code{presolve} is \code{"none"} then no presolving is done.

      \tabular{rl}{
        \code{"none"}: \tab No presolve. \cr
        \code{"rows"}: \tab Presolve rows. \cr
        \code{"cols"}: \tab Presolve columns. \cr
        \code{"lindep"}: \tab Eliminate linearly dependent rows. \cr
        \code{"sos"}: \tab Convert constraints to special ordered sets (SOS), only SOS1 is handled. \cr
        \code{"reducemip"}: \tab Constraints found redundant in phase 1 are deleted. This is no longer active since it is rarely effective and also because it adds code complications and delayed presolve effects that are not captured properly. \cr
        \code{"knapsack"}: \tab Simplification of knapsack-type constraints through the addition of an extra variable. This also helps bound the objective function. \cr
        \code{"elimeq2"}: \tab Direct substitution of one variable in 2-element equality constraints; this requires changes to the constraint matrix. \cr
        \code{"impliedfree"}: \tab Identify implied free variables (releasing their explicit bounds). \cr
        \code{"reducegcd"}: \tab Reduce (tighten) coefficients in integer models based on GCD argument. \cr
        \code{"probefix"}: \tab Attempt to fix binary variables at one of their bounds. \cr
        \code{"probereduce"}: \tab Attempt to reduce coefficients in binary models. \cr
        \code{"rowdominate"}: \tab Identify and delete qualifying constraints that are dominated by others, also fixes variables at a bound. \cr
        \code{"coldominate"}: \tab Delete variables (mainly binary) that are dominated by others (only one can be non-zero). \cr
        \code{"mergerows"}: \tab Merges neighboring \code{>=} or \code{<=} constraints when the vectors are otherwise relatively identical into a single ranged constraint. \cr
        \code{"impliedslk"}: \tab Converts qualifying equalities to inequalities by converting a column singleton variable to a slack variable. The routine also detects implicit duplicate slacks from inequality constraints and fixes and removes the redundant variable. This removal also tends to reduce the risk of degeneracy. The combined function of this option can have a dramatic simplifying effect on some models. \cr
        \code{"colfixdual"}: \tab Variable fixing and removal based on the signs of the associated dual constraint. \cr
        \code{"bounds"}: \tab Bound tightening based on full-row constraint information. This can assist in tightening the objective function bound, eliminate variables and constraints. At the end of presolve, it is checked if any variables can be deemed free, thereby reducing any chance that degeneracy is introduced via this presolve option. \cr
        \code{"duals"}: \tab Calculate duals. \cr
        \code{"sensduals"}: \tab Calculate sensitivity if there are integer variables. \cr
      }

      The default is \code{c("none")}.}

    \item{scalelimit}{a numeric value specifying the relative scaling convergence criterion for the active scaling mode; the integer part specifies the maximum number of iterations. The default is \code{5}.}

    \item{scaling}{a character vector specifying the scaling algorithm used and zero or more augmentations. The first element must be one of the scaling algorithms given in the following table.

      \tabular{rl}{
        \code{"none"}: \tab No scaling (not advised). \cr
        \code{"extreme"}: \tab Scale to convergence using largest absolute value. \cr
        \code{"range"}: \tab Scale based on the simple numerical range. \cr
        \code{"mean"}: \tab Numerical range-based scaling. \cr
        \code{"geometric"}: \tab Geometric scaling. \cr
        \code{"curtisreid"}: \tab Curtis-Reid scaling. \cr
      }

      Additional elements (if any) from the following table can be included to augment the scaling algorithm.

      \tabular{rl}{
        \code{"quadratic"}: \tab  \cr
        \code{"logarithmic"}: \tab Scale to convergence using logarithmic mean of all values. \cr
        \code{"power2"}: \tab Power scaling. \cr
        \code{"equilibrate"}: \tab Make sure that no scaled number is above \code{1}. \cr
        \code{"integers"}: \tab Scale integer variables. \cr
        \code{"dynupdate"}: \tab Recompute scale factors when resolving the model. \cr
        \code{"rowsonly"}: \tab Only scale rows. \cr
        \code{"colsonly"}: \tab Only scale columns. \cr
      }

      By default, lpSolve computes scale factors once for the original model. If a solve is done again (most probably after changing some data in the model), the scaling factors are not recomputed. Instead, the scale factors from the original model are used. This is not always desirable, especially if the data has changed considerably. Including \code{"dynupdate"} among the scale algorithm augmentations instructs lpSolve to recompute the scale factors each time \code{solve} is called. Note that the scaling done by \code{"dynupdate"} is incremental and the resulting scale factors are typically different from those computed from scratch.

      The default is \code{c("geometric", "equilibrate", "integers")}.}

    \item{sense}{one of \code{"max"} or \code{"min"} specifying whether the model is a maximization or a minimization problem.}

    \item{simplextype}{a character vector of length one or two composed of \code{"primal"} and \code{"dual"}. If length two then the first element describes the simplex type used in phase 1 and the second element the simplex type used in phase 2. If length one then that simplex type is used for both phases. The default is \code{c("dual", "primal")}.}

    \item{timeout}{a positive integer value specifying the number of seconds after which a timeout will occur. If zero, then no timeout will occur.}

    \item{verbose}{a character string controlling the severity of messages reported by lp_solve. The possible choices are given in the table below. All errors/warnings in lp_solve have a particular severiy: for example, specifying a wrong row or column index is considered a severe error. All messages equal to and below the set level are reported (in the console).

      \tabular{rl}{
        \code{"neutral"}: \tab No reporting. \cr
        \code{"critical"}: \tab Only critical messages are reported. Hard errors like instability, out of memory, etc. \cr
        \code{"severe"}: \tab Only severe messages are reported. Errors. \cr
        \code{"important"}: \tab Only important messages are reported. Warnings and Errors. \cr
        \code{"normal"}: \tab Normal messages are reported. \cr
        \code{"detailed"}: \tab Detailed messages are reported. Like model size, continuing B&B improvements, etc. \cr
        \code{"full"}: \tab All messages are reported. Useful for debugging purposes and small models. \cr
      }
    }
  }
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{lp.control}}}

\keyword{programming}


