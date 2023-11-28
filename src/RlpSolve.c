#include "RlpSolve.h"
#include "RlpSolveLink.h"

SEXP RlpSolve_lprec_tag;

lprec* lprecPointerFromSEXP(SEXP Slprec)
{
  if(R_ExternalPtrAddr(Slprec) == NULL)
    error("NULL value passed as linear program record");

  if(TYPEOF(Slprec) != EXTPTRSXP || R_ExternalPtrTag(Slprec) != RlpSolve_lprec_tag)
    error("the lp argument does not appear to be a valid linear program record");

  return (lprec*) R_ExternalPtrAddr(Slprec);
}


void RlpsHS(lprec *lp, unsigned char status)
{
  if(status)
    return;

  error("%s", get_statustext(lp, get_status(lp)));
}


void __WINAPI RlpSolveLogFunction(lprec *lp, void *userhandle, char *buf)
{
  Rprintf("%s", buf);
}


int __WINAPI RlpSolveAbortFunction(lprec *lp, void *userhandle)
{
  int abort = 0;
  /*if(R_ToplevelExec((void *) R_CheckUserInterrupt, NULL)) {
    Rprintf("R_CheckUserInterrupt returned successfully.\n");
    abort = 0;
  }
  else {
    Rprintf("\n*************************************************\n");
    Rprintf("R_CheckUserInterrupt failed to return.");
    Rprintf("\n*************************************************\n");
    abort = 1;
  }*/
  return(abort);
}


void R_init_lpSolveAPI(DllInfo *info)
{
  const char package[] = "lpSolveAPI";
  RlpSolve_lprec_tag = install("RLPSOLVE_LPREC_TAG");

  R_CallMethodDef dotCallMethods[] = {
    {"RlpSolve_make_lp", (DL_FUNC) RlpSolve_make_lp, 2},
    {"RlpSolve_copy_lp", (DL_FUNC) RlpSolve_copy_lp, 1},
    {"RlpSolve_read_LP", (DL_FUNC) RlpSolve_read_LP, 2},
    {"RlpSolve_read_MPS", (DL_FUNC) RlpSolve_read_MPS, 2},
    {"RlpSolve_read_freeMPS", (DL_FUNC) RlpSolve_read_freeMPS, 2},
    {"RlpSolve_delete_lp", (DL_FUNC) RlpSolve_delete_lp, 1},
    {"RlpSolve_add_columnex", (DL_FUNC) RlpSolve_add_columnex, 3},
    {"RlpSolve_set_columnex", (DL_FUNC) RlpSolve_set_columnex, 4},
    {"RlpSolve_get_columnex", (DL_FUNC) RlpSolve_get_columnex, 2},
    {"RlpSolve_add_constraintex", (DL_FUNC) RlpSolve_add_constraintex, 5},
    {"RlpSolve_set_rowex", (DL_FUNC) RlpSolve_set_rowex, 4},
    {"RlpSolve_add_lag_con", (DL_FUNC) RlpSolve_add_lag_con, 4},
    {"RlpSolve_add_SOS", (DL_FUNC) RlpSolve_add_SOS, 6},
    {"RlpSolve_is_SOS_var", (DL_FUNC) RlpSolve_is_SOS_var, 2},
    {"RlpSolve_del_column", (DL_FUNC) RlpSolve_del_column, 2},
    {"RlpSolve_del_constraint", (DL_FUNC) RlpSolve_del_constraint, 2},
    {"RlpSolve_get_rowex", (DL_FUNC) RlpSolve_get_rowex, 2},
    {"RlpSolve_get_nameindex", (DL_FUNC) RlpSolve_get_nameindex, 3},
    {"RlpSolve_is_infinite", (DL_FUNC) RlpSolve_is_infinite, 2},
    {"RlpSolve_is_negative", (DL_FUNC) RlpSolve_is_negative, 2},
    {"RlpSolve_resize_lp", (DL_FUNC) RlpSolve_resize_lp, 3},
    {"RlpSolve_set_add_rowmode", (DL_FUNC) RlpSolve_set_add_rowmode, 2},
    {"RlpSolve_is_add_rowmode", (DL_FUNC) RlpSolve_is_add_rowmode, 1},
    {"RlpSolve_set_binary", (DL_FUNC) RlpSolve_set_binary, 3},
    {"RlpSolve_is_binary", (DL_FUNC) RlpSolve_is_binary, 2},
    {"RlpSolve_set_bounds", (DL_FUNC) RlpSolve_set_bounds, 4},
    {"RlpSolve_set_bounds_tighter", (DL_FUNC) RlpSolve_set_bounds_tighter, 2},
    {"RlpSolve_get_bounds_tighter", (DL_FUNC) RlpSolve_get_bounds_tighter, 1},
    {"RlpSolve_set_col_names", (DL_FUNC) RlpSolve_set_col_names, 3},
    {"RlpSolve_get_col_names", (DL_FUNC) RlpSolve_get_col_names, 2},
    {"RlpSolve_get_origcol_names", (DL_FUNC) RlpSolve_get_origcol_names, 2},
    {"RlpSolve_set_constr_type", (DL_FUNC) RlpSolve_set_constr_type, 3},
    {"RlpSolve_get_constr_type", (DL_FUNC) RlpSolve_get_constr_type, 2},
    {"RlpSolve_is_constr_type", (DL_FUNC) RlpSolve_is_constr_type, 3}, 
    {"RlpSolve_set_unbounded", (DL_FUNC) RlpSolve_set_unbounded, 2},
    {"RlpSolve_is_unbounded", (DL_FUNC) RlpSolve_is_unbounded, 2},
    {"RlpSolve_set_infinite", (DL_FUNC) RlpSolve_set_infinite, 2},
    {"RlpSolve_get_infinite", (DL_FUNC) RlpSolve_get_infinite, 1},
    {"RlpSolve_set_int", (DL_FUNC) RlpSolve_set_int, 3},
    {"RlpSolve_is_int", (DL_FUNC) RlpSolve_is_int, 2},
    {"RlpSolve_set_lowbo", (DL_FUNC) RlpSolve_set_lowbo, 3},
    {"RlpSolve_get_lowbo", (DL_FUNC) RlpSolve_get_lowbo, 2},
    {"RlpSolve_set_lp_name", (DL_FUNC) RlpSolve_set_lp_name, 2},
    {"RlpSolve_get_lp_name", (DL_FUNC) RlpSolve_get_lp_name, 1},
    {"RlpSolve_set_mat", (DL_FUNC) RlpSolve_set_mat, 4},
    {"RlpSolve_get_mat", (DL_FUNC) RlpSolve_get_mat, 3},
    {"RlpSolve_set_obj_bound", (DL_FUNC) RlpSolve_set_obj_bound, 2},
    {"RlpSolve_get_obj_bound", (DL_FUNC) RlpSolve_get_obj_bound, 1},
    {"RlpSolve_set_obj_fnex", (DL_FUNC) RlpSolve_set_obj_fnex, 3},
    {"RlpSolve_set_rh", (DL_FUNC) RlpSolve_set_rh, 3},
    {"RlpSolve_get_rh", (DL_FUNC) RlpSolve_get_rh, 2},
    {"RlpSolve_set_rh_range", (DL_FUNC) RlpSolve_set_rh_range, 3},
    {"RlpSolve_get_rh_range", (DL_FUNC) RlpSolve_get_rh_range, 2},
    {"RlpSolve_set_rh_vec", (DL_FUNC) RlpSolve_set_rh_vec, 2},
    {"RlpSolve_set_row_names", (DL_FUNC) RlpSolve_set_row_names, 3},
    {"RlpSolve_get_row_names", (DL_FUNC) RlpSolve_get_row_names, 2},
    {"RlpSolve_get_origrow_names", (DL_FUNC) RlpSolve_get_origrow_names, 2},
    {"RlpSolve_set_semicont", (DL_FUNC) RlpSolve_set_semicont, 3},
    {"RlpSolve_is_semicont", (DL_FUNC) RlpSolve_is_semicont, 2},
    {"RlpSolve_set_upbo", (DL_FUNC) RlpSolve_set_upbo, 3},
    {"RlpSolve_get_upbo", (DL_FUNC) RlpSolve_get_upbo, 2},
    {"RlpSolve_set_var_branch", (DL_FUNC) RlpSolve_set_var_branch, 3},
    {"RlpSolve_get_var_branch", (DL_FUNC) RlpSolve_get_var_branch, 2},
    {"RlpSolve_set_var_weights", (DL_FUNC) RlpSolve_set_var_weights, 2},
    {"RlpSolve_default_basis", (DL_FUNC) RlpSolve_default_basis, 1},
    {"RlpSolve_reset_basis", (DL_FUNC) RlpSolve_reset_basis, 1},
    {"RlpSolve_guess_basis", (DL_FUNC) RlpSolve_guess_basis, 2},
    {"RlpSolve_reset_params", (DL_FUNC) RlpSolve_reset_params, 1},
    {"RlpSolve_set_anti_degen", (DL_FUNC) RlpSolve_set_anti_degen, 2},
    {"RlpSolve_is_anti_degen", (DL_FUNC) RlpSolve_is_anti_degen, 2},
    {"RlpSolve_set_basis", (DL_FUNC) RlpSolve_set_basis, 3},
    {"RlpSolve_get_basis", (DL_FUNC) RlpSolve_get_basis, 2},
    {"RlpSolve_set_basiscrash", (DL_FUNC) RlpSolve_set_basiscrash, 2},
    {"RlpSolve_get_basiscrash", (DL_FUNC) RlpSolve_get_basiscrash, 1},
    {"RlpSolve_set_bb_depthlimit", (DL_FUNC) RlpSolve_set_bb_depthlimit, 2},
    {"RlpSolve_get_bb_depthlimit", (DL_FUNC) RlpSolve_get_bb_depthlimit, 1},
    {"RlpSolve_set_bb_floorfirst", (DL_FUNC) RlpSolve_set_bb_floorfirst, 2},
    {"RlpSolve_get_bb_floorfirst", (DL_FUNC) RlpSolve_get_bb_floorfirst, 1},
    {"RlpSolve_set_bb_rule", (DL_FUNC) RlpSolve_set_bb_rule, 2},
    {"RlpSolve_get_bb_rule", (DL_FUNC) RlpSolve_get_bb_rule, 1},
    {"RlpSolve_set_break_at_first", (DL_FUNC) RlpSolve_set_break_at_first, 2},
    {"RlpSolve_is_break_at_first", (DL_FUNC) RlpSolve_is_break_at_first, 1},
    {"RlpSolve_set_break_at_value", (DL_FUNC) RlpSolve_set_break_at_value, 2},
    {"RlpSolve_get_break_at_value", (DL_FUNC) RlpSolve_get_break_at_value, 1},
    {"RlpSolve_set_epsb", (DL_FUNC) RlpSolve_set_epsb, 2},
    {"RlpSolve_get_epsb", (DL_FUNC) RlpSolve_get_epsb, 1},
    {"RlpSolve_set_epsd", (DL_FUNC) RlpSolve_set_epsd, 2},
    {"RlpSolve_get_epsd", (DL_FUNC) RlpSolve_get_epsd, 1},
    {"RlpSolve_set_epsel", (DL_FUNC) RlpSolve_set_epsel, 2},
    {"RlpSolve_get_epsel", (DL_FUNC) RlpSolve_get_epsel, 1},
    {"RlpSolve_set_epsint", (DL_FUNC) RlpSolve_set_epsint, 2},
    {"RlpSolve_get_epsint", (DL_FUNC) RlpSolve_get_epsint, 1},
    {"RlpSolve_set_epsperturb", (DL_FUNC) RlpSolve_set_epsperturb, 2},
    {"RlpSolve_get_epsperturb", (DL_FUNC) RlpSolve_get_epsperturb, 1},
    {"RlpSolve_set_epspivot", (DL_FUNC) RlpSolve_set_epspivot, 2},
    {"RlpSolve_get_epspivot", (DL_FUNC) RlpSolve_get_epspivot, 1},
    {"RlpSolve_set_epslevel", (DL_FUNC) RlpSolve_set_epslevel, 2},
    {"RlpSolve_set_improve", (DL_FUNC) RlpSolve_set_improve, 2},
    {"RlpSolve_get_improve", (DL_FUNC) RlpSolve_get_improve, 1},
    {"RlpSolve_set_maxim", (DL_FUNC) RlpSolve_set_maxim, 1},
    {"RlpSolve_is_maxim", (DL_FUNC) RlpSolve_is_maxim, 1},
    {"RlpSolve_set_maxpivot", (DL_FUNC) RlpSolve_set_maxpivot, 2},
    {"RlpSolve_get_maxpivot", (DL_FUNC) RlpSolve_get_maxpivot, 1},
    {"RlpSolve_set_minim", (DL_FUNC) RlpSolve_set_minim, 1},
    {"RlpSolve_set_mip_gap", (DL_FUNC) RlpSolve_set_mip_gap, 3},
    {"RlpSolve_get_mip_gap", (DL_FUNC) RlpSolve_get_mip_gap, 2},
    {"RlpSolve_set_negrange", (DL_FUNC) RlpSolve_set_negrange, 2},
    {"RlpSolve_get_negrange", (DL_FUNC) RlpSolve_get_negrange, 1},
    {"RlpSolve_set_obj_in_basis", (DL_FUNC) RlpSolve_set_obj_in_basis, 2},
    {"RlpSolve_is_obj_in_basis", (DL_FUNC) RlpSolve_is_obj_in_basis, 1},
    {"RlpSolve_set_pivoting", (DL_FUNC) RlpSolve_set_pivoting, 2},
    {"RlpSolve_get_pivoting", (DL_FUNC) RlpSolve_get_pivoting, 1},
    {"RlpSolve_is_piv_mode", (DL_FUNC) RlpSolve_is_piv_mode, 2},
    {"RlpSolve_is_piv_rule", (DL_FUNC) RlpSolve_is_piv_rule, 2},
    {"RlpSolve_set_preferdual", (DL_FUNC) RlpSolve_set_preferdual, 2},
    {"RlpSolve_set_presolve", (DL_FUNC) RlpSolve_set_presolve, 3},
    {"RlpSolve_get_presolve", (DL_FUNC) RlpSolve_get_presolve, 1},
    {"RlpSolve_get_presolveloops", (DL_FUNC) RlpSolve_get_presolveloops, 1},
    {"RlpSolve_is_presolve", (DL_FUNC) RlpSolve_is_presolve, 2},
    {"RlpSolve_set_scalelimit", (DL_FUNC) RlpSolve_set_scalelimit, 2},
    {"RlpSolve_get_scalelimit", (DL_FUNC) RlpSolve_get_scalelimit, 1},
    {"RlpSolve_set_scaling", (DL_FUNC) RlpSolve_set_scaling, 2},
    {"RlpSolve_get_scaling", (DL_FUNC) RlpSolve_get_scaling, 1},
    {"RlpSolve_is_integerscaling", (DL_FUNC) RlpSolve_is_integerscaling, 1},
    {"RlpSolve_is_scalemode", (DL_FUNC) RlpSolve_is_scalemode, 2},
    {"RlpSolve_is_scaletype", (DL_FUNC) RlpSolve_is_scaletype, 2},
    {"RlpSolve_set_sense", (DL_FUNC) RlpSolve_set_sense, 2},
    {"RlpSolve_set_simplextype", (DL_FUNC) RlpSolve_set_simplextype, 2},
    {"RlpSolve_get_simplextype", (DL_FUNC) RlpSolve_get_simplextype, 1},
    {"RlpSolve_set_solutionlimit", (DL_FUNC) RlpSolve_set_solutionlimit, 2},
    {"RlpSolve_get_solutionlimit", (DL_FUNC) RlpSolve_get_solutionlimit, 1},
    {"RlpSolve_set_timeout", (DL_FUNC) RlpSolve_set_timeout, 2},
    {"RlpSolve_get_timeout", (DL_FUNC) RlpSolve_get_timeout, 1},
    {"RlpSolve_set_use_names", (DL_FUNC) RlpSolve_set_use_names, 3},
    {"RlpSolve_is_use_names", (DL_FUNC) RlpSolve_is_use_names, 2},
    {"RlpSolve_unscale", (DL_FUNC) RlpSolve_unscale, 1},
    {"RlpSolve_solve", (DL_FUNC) RlpSolve_solve, 1},
    {"RlpSolve_get_constraints", (DL_FUNC) RlpSolve_get_constraints, 1},
    {"RlpSolve_get_objective", (DL_FUNC) RlpSolve_get_objective, 1},
    {"RlpSolve_get_primal_solution", (DL_FUNC) RlpSolve_get_primal_solution, 1},
    {"RlpSolve_get_var_primalresult", (DL_FUNC) RlpSolve_get_var_primalresult, 1},
    {"RlpSolve_get_sensitivity_obj", (DL_FUNC) RlpSolve_get_sensitivity_obj, 1},
    {"RlpSolve_get_sensitivity_objex", (DL_FUNC) RlpSolve_get_sensitivity_objex, 1},
    {"RlpSolve_get_sensitivity_rhs", (DL_FUNC) RlpSolve_get_sensitivity_rhs, 1},
    {"RlpSolve_get_dual_solution", (DL_FUNC) RlpSolve_get_dual_solution, 1},
    {"RlpSolve_get_solutioncount", (DL_FUNC) RlpSolve_get_solutioncount, 1},
    {"RlpSolve_get_total_iter", (DL_FUNC) RlpSolve_get_total_iter, 1},
    {"RlpSolve_get_total_nodes", (DL_FUNC) RlpSolve_get_total_nodes, 1},
    {"RlpSolve_get_variables", (DL_FUNC) RlpSolve_get_variables, 1},
    {"RlpSolve_set_verbose", (DL_FUNC) RlpSolve_set_verbose, 2},
    {"RlpSolve_get_verbose", (DL_FUNC) RlpSolve_get_verbose, 1},
    {"RlpSolve_write_lp", (DL_FUNC) RlpSolve_write_lp, 2},
    {"RlpSolve_write_mps", (DL_FUNC) RlpSolve_write_mps, 2},
    {"RlpSolve_write_freemps", (DL_FUNC) RlpSolve_write_freemps, 2},
    {"RlpSolve_dualize_lp", (DL_FUNC) RlpSolve_dualize_lp, 1},
    {"RlpSolve_get_lp_index", (DL_FUNC) RlpSolve_get_lp_index, 2},
    /*{"RlpSolve_get_Lrows", (DL_FUNC) RlpSolve_get_Lrows, 1},*/
    {"RlpSolve_get_Ncolumns", (DL_FUNC) RlpSolve_get_Ncolumns, 1},
    {"RlpSolve_get_nonzeros", (DL_FUNC) RlpSolve_get_nonzeros, 1},
    {"RlpSolve_get_Norig_columns", (DL_FUNC) RlpSolve_get_Norig_columns, 1},
    {"RlpSolve_get_Norig_rows", (DL_FUNC) RlpSolve_get_Norig_rows, 1},
    {"RlpSolve_get_Nrows", (DL_FUNC) RlpSolve_get_Nrows, 1},
    {"RlpSolve_get_orig_index", (DL_FUNC) RlpSolve_get_orig_index, 2},
    {"RlpSolve_get_status", (DL_FUNC) RlpSolve_get_status, 1},
    {"RlpSolve_get_statustext", (DL_FUNC) RlpSolve_get_statustext, 2},
    {"RlpSolve_lp_solve_version", (DL_FUNC) RlpSolve_lp_solve_version, 0},
    {"RlpSolve_time_elapsed", (DL_FUNC) RlpSolve_time_elapsed, 1},
    {NULL, NULL, 0}};

  R_registerRoutines(info, NULL, dotCallMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);

  R_RegisterCCallable(package, "lp_solve_version", (DL_FUNC) lp_solve_version);
  R_RegisterCCallable(package, "make_lp", (DL_FUNC) make_lp);
  R_RegisterCCallable(package, "resize_lp", (DL_FUNC) resize_lp);
  R_RegisterCCallable(package, "get_status", (DL_FUNC) get_status);
  R_RegisterCCallable(package, "get_statustext", (DL_FUNC) get_statustext);
  R_RegisterCCallable(package, "is_obj_in_basis", (DL_FUNC) is_obj_in_basis);
  R_RegisterCCallable(package, "set_obj_in_basis", (DL_FUNC) set_obj_in_basis);
  R_RegisterCCallable(package, "copy_lp", (DL_FUNC) copy_lp);
  R_RegisterCCallable(package, "dualize_lp", (DL_FUNC) dualize_lp);
  R_RegisterCCallable(package, "delete_lp", (DL_FUNC) delete_lp);
  R_RegisterCCallable(package, "free_lp", (DL_FUNC) free_lp);
  R_RegisterCCallable(package, "set_lp_name", (DL_FUNC) set_lp_name);
  R_RegisterCCallable(package, "get_lp_name", (DL_FUNC) get_lp_name);
  R_RegisterCCallable(package, "has_BFP", (DL_FUNC) has_BFP);
  R_RegisterCCallable(package, "is_nativeBFP", (DL_FUNC) is_nativeBFP);
  R_RegisterCCallable(package, "set_BFP", (DL_FUNC) set_BFP);
  R_RegisterCCallable(package, "read_XLI", (DL_FUNC) read_XLI);
  R_RegisterCCallable(package, "write_XLI", (DL_FUNC) write_XLI);
  R_RegisterCCallable(package, "has_XLI", (DL_FUNC) has_XLI);
  R_RegisterCCallable(package, "is_nativeXLI", (DL_FUNC) is_nativeXLI);
  R_RegisterCCallable(package, "set_XLI", (DL_FUNC) set_XLI);
  R_RegisterCCallable(package, "set_obj", (DL_FUNC) set_obj);
  R_RegisterCCallable(package, "set_obj_fn", (DL_FUNC) set_obj_fn);
  R_RegisterCCallable(package, "set_obj_fnex", (DL_FUNC) set_obj_fnex);
  R_RegisterCCallable(package, "str_set_obj_fn", (DL_FUNC) str_set_obj_fn);
  R_RegisterCCallable(package, "set_sense", (DL_FUNC) set_sense);
  R_RegisterCCallable(package, "set_maxim", (DL_FUNC) set_maxim);
  R_RegisterCCallable(package, "set_minim", (DL_FUNC) set_minim);
  R_RegisterCCallable(package, "is_maxim", (DL_FUNC) is_maxim);
  R_RegisterCCallable(package, "add_constraint", (DL_FUNC) add_constraint);
  R_RegisterCCallable(package, "add_constraintex", (DL_FUNC) add_constraintex);
  R_RegisterCCallable(package, "set_add_rowmode", (DL_FUNC) set_add_rowmode);
  R_RegisterCCallable(package, "is_add_rowmode", (DL_FUNC) is_add_rowmode);
  R_RegisterCCallable(package, "str_add_constraint", (DL_FUNC) str_add_constraint);
  R_RegisterCCallable(package, "set_row", (DL_FUNC) set_row);
  R_RegisterCCallable(package, "set_rowex", (DL_FUNC) set_rowex);
  R_RegisterCCallable(package, "get_row", (DL_FUNC) get_row);
  R_RegisterCCallable(package, "get_rowex", (DL_FUNC) get_rowex);
  R_RegisterCCallable(package, "del_constraint", (DL_FUNC) del_constraint);
  R_RegisterCCallable(package, "del_constraintex", (DL_FUNC) del_constraintex);
  R_RegisterCCallable(package, "add_lag_con", (DL_FUNC) add_lag_con);
  R_RegisterCCallable(package, "str_add_lag_con", (DL_FUNC) str_add_lag_con);
  R_RegisterCCallable(package, "set_lag_trace", (DL_FUNC) set_lag_trace);
  R_RegisterCCallable(package, "is_lag_trace", (DL_FUNC) is_lag_trace);
  R_RegisterCCallable(package, "set_constr_type", (DL_FUNC) set_constr_type);
  R_RegisterCCallable(package, "get_constr_type", (DL_FUNC) get_constr_type);
  R_RegisterCCallable(package, "get_constr_value", (DL_FUNC) get_constr_value);
  R_RegisterCCallable(package, "is_constr_type", (DL_FUNC) is_constr_type);
  R_RegisterCCallable(package, "get_str_constr_type", (DL_FUNC) get_str_constr_type);
  R_RegisterCCallable(package, "get_constr_class", (DL_FUNC) get_constr_class);
  R_RegisterCCallable(package, "get_str_constr_class", (DL_FUNC) get_str_constr_class);
  R_RegisterCCallable(package, "set_rh", (DL_FUNC) set_rh);
  R_RegisterCCallable(package, "get_rh", (DL_FUNC) get_rh);
  R_RegisterCCallable(package, "set_rh_range", (DL_FUNC) set_rh_range);
  R_RegisterCCallable(package, "get_rh_range", (DL_FUNC) get_rh_range);
  R_RegisterCCallable(package, "set_rh_vec", (DL_FUNC) set_rh_vec);
  R_RegisterCCallable(package, "str_set_rh_vec", (DL_FUNC) str_set_rh_vec);
  R_RegisterCCallable(package, "add_column", (DL_FUNC) add_column);
  R_RegisterCCallable(package, "add_columnex", (DL_FUNC) add_columnex);
  R_RegisterCCallable(package, "str_add_column", (DL_FUNC) str_add_column);
  R_RegisterCCallable(package, "set_column", (DL_FUNC) set_column);
  R_RegisterCCallable(package, "set_columnex", (DL_FUNC) set_columnex);
  R_RegisterCCallable(package, "column_in_lp", (DL_FUNC) column_in_lp);
  R_RegisterCCallable(package, "get_columnex", (DL_FUNC) get_columnex);
  R_RegisterCCallable(package, "get_column", (DL_FUNC) get_column);
  R_RegisterCCallable(package, "del_column", (DL_FUNC) del_column);
  R_RegisterCCallable(package, "del_columnex", (DL_FUNC) del_columnex);
  R_RegisterCCallable(package, "set_mat", (DL_FUNC) set_mat);
  R_RegisterCCallable(package, "get_mat", (DL_FUNC) get_mat);
  R_RegisterCCallable(package, "get_mat_byindex", (DL_FUNC) get_mat_byindex);
  R_RegisterCCallable(package, "get_nonzeros", (DL_FUNC) get_nonzeros);
  R_RegisterCCallable(package, "set_bounds_tighter", (DL_FUNC) set_bounds_tighter);
  R_RegisterCCallable(package, "get_bounds", (DL_FUNC) get_bounds);
  R_RegisterCCallable(package, "get_bounds_tighter", (DL_FUNC) get_bounds_tighter);
  R_RegisterCCallable(package, "set_upbo", (DL_FUNC) set_upbo);
  R_RegisterCCallable(package, "get_upbo", (DL_FUNC) get_upbo);
  R_RegisterCCallable(package, "set_lowbo", (DL_FUNC) set_lowbo);
  R_RegisterCCallable(package, "get_lowbo", (DL_FUNC) get_lowbo);
  R_RegisterCCallable(package, "set_bounds", (DL_FUNC) set_bounds);
  R_RegisterCCallable(package, "set_unbounded", (DL_FUNC) set_unbounded);
  R_RegisterCCallable(package, "is_unbounded", (DL_FUNC) is_unbounded);
  R_RegisterCCallable(package, "set_int", (DL_FUNC) set_int);
  R_RegisterCCallable(package, "is_int", (DL_FUNC) is_int);
  R_RegisterCCallable(package, "set_binary", (DL_FUNC) set_binary);
  R_RegisterCCallable(package, "is_binary", (DL_FUNC) is_binary);
  R_RegisterCCallable(package, "set_semicont", (DL_FUNC) set_semicont);
  R_RegisterCCallable(package, "is_semicont", (DL_FUNC) is_semicont);
  R_RegisterCCallable(package, "is_negative", (DL_FUNC) is_negative);
  R_RegisterCCallable(package, "set_var_weights", (DL_FUNC) set_var_weights);
  R_RegisterCCallable(package, "get_var_priority", (DL_FUNC) get_var_priority);
  R_RegisterCCallable(package, "set_pseudocosts", (DL_FUNC) set_pseudocosts);
  R_RegisterCCallable(package, "get_pseudocosts", (DL_FUNC) get_pseudocosts);
  R_RegisterCCallable(package, "add_SOS", (DL_FUNC) add_SOS);
  R_RegisterCCallable(package, "is_SOS_var", (DL_FUNC) is_SOS_var);
  R_RegisterCCallable(package, "set_row_name", (DL_FUNC) set_row_name);
  R_RegisterCCallable(package, "get_row_name", (DL_FUNC) get_row_name);
  R_RegisterCCallable(package, "get_origrow_name", (DL_FUNC) get_origrow_name);
  R_RegisterCCallable(package, "set_col_name", (DL_FUNC) set_col_name);
  R_RegisterCCallable(package, "get_col_name", (DL_FUNC) get_col_name);
  R_RegisterCCallable(package, "get_origcol_name", (DL_FUNC) get_origcol_name);
  R_RegisterCCallable(package, "unscale", (DL_FUNC) unscale);
  R_RegisterCCallable(package, "set_preferdual", (DL_FUNC) set_preferdual);
  R_RegisterCCallable(package, "set_simplextype", (DL_FUNC) set_simplextype);
  R_RegisterCCallable(package, "get_simplextype", (DL_FUNC) get_simplextype);
  R_RegisterCCallable(package, "default_basis", (DL_FUNC) default_basis);
  R_RegisterCCallable(package, "set_basiscrash", (DL_FUNC) set_basiscrash);
  R_RegisterCCallable(package, "get_basiscrash", (DL_FUNC) get_basiscrash);
  R_RegisterCCallable(package, "set_basisvar", (DL_FUNC) set_basisvar);
  R_RegisterCCallable(package, "set_basis", (DL_FUNC) set_basis);
  R_RegisterCCallable(package, "get_basis", (DL_FUNC) get_basis);
  R_RegisterCCallable(package, "reset_basis", (DL_FUNC) reset_basis);
  R_RegisterCCallable(package, "guess_basis", (DL_FUNC) guess_basis);
  R_RegisterCCallable(package, "is_feasible", (DL_FUNC) is_feasible);
  R_RegisterCCallable(package, "solve", (DL_FUNC) solve);
  R_RegisterCCallable(package, "time_elapsed", (DL_FUNC) time_elapsed);
  R_RegisterCCallable(package, "put_bb_nodefunc", (DL_FUNC) put_bb_nodefunc);
  R_RegisterCCallable(package, "put_bb_branchfunc", (DL_FUNC) put_bb_branchfunc);
  R_RegisterCCallable(package, "put_abortfunc", (DL_FUNC) put_abortfunc);
  R_RegisterCCallable(package, "put_logfunc", (DL_FUNC) put_logfunc);
  R_RegisterCCallable(package, "put_msgfunc", (DL_FUNC) put_msgfunc);
  R_RegisterCCallable(package, "get_primal_solution", (DL_FUNC) get_primal_solution);
  R_RegisterCCallable(package, "get_ptr_primal_solution", (DL_FUNC) get_ptr_primal_solution);
  R_RegisterCCallable(package, "get_dual_solution", (DL_FUNC) get_dual_solution);
  R_RegisterCCallable(package, "get_ptr_dual_solution", (DL_FUNC) get_ptr_dual_solution);
  R_RegisterCCallable(package, "get_lambda", (DL_FUNC) get_lambda);
  R_RegisterCCallable(package, "get_ptr_lambda", (DL_FUNC) get_ptr_lambda);
  R_RegisterCCallable(package, "read_MPS", (DL_FUNC) read_MPS);
  R_RegisterCCallable(package, "read_mps", (DL_FUNC) read_mps);
  R_RegisterCCallable(package, "read_freeMPS", (DL_FUNC) read_freeMPS);
  R_RegisterCCallable(package, "read_freemps", (DL_FUNC) read_freemps);
  R_RegisterCCallable(package, "write_mps", (DL_FUNC) write_mps);
  R_RegisterCCallable(package, "write_MPS", (DL_FUNC) write_MPS);
  R_RegisterCCallable(package, "write_freemps", (DL_FUNC) write_freemps);
  R_RegisterCCallable(package, "write_freeMPS", (DL_FUNC) write_freeMPS);
  R_RegisterCCallable(package, "write_lp", (DL_FUNC) write_lp);
  R_RegisterCCallable(package, "write_LP", (DL_FUNC) write_LP);
  R_RegisterCCallable(package, "LP_readhandle", (DL_FUNC) LP_readhandle);
  R_RegisterCCallable(package, "read_lp", (DL_FUNC) read_lp);
  R_RegisterCCallable(package, "read_LP", (DL_FUNC) read_LP);
  R_RegisterCCallable(package, "write_basis", (DL_FUNC) write_basis);
  R_RegisterCCallable(package, "read_basis", (DL_FUNC) read_basis);
  R_RegisterCCallable(package, "write_params", (DL_FUNC) write_params);
  R_RegisterCCallable(package, "read_params", (DL_FUNC) read_params);
  R_RegisterCCallable(package, "reset_params", (DL_FUNC) reset_params);
  R_RegisterCCallable(package, "print_lp", (DL_FUNC) print_lp);
  R_RegisterCCallable(package, "print_tableau", (DL_FUNC) print_tableau);
  R_RegisterCCallable(package, "print_objective", (DL_FUNC) print_objective);
  R_RegisterCCallable(package, "print_solution", (DL_FUNC) print_solution);
  R_RegisterCCallable(package, "print_constraints", (DL_FUNC) print_constraints);
  R_RegisterCCallable(package, "print_duals", (DL_FUNC) print_duals);
  R_RegisterCCallable(package, "print_scales", (DL_FUNC) print_scales);
  R_RegisterCCallable(package, "print_str", (DL_FUNC) print_str);
  R_RegisterCCallable(package, "set_outputstream", (DL_FUNC) set_outputstream);
  R_RegisterCCallable(package, "set_outputfile", (DL_FUNC) set_outputfile);
  R_RegisterCCallable(package, "set_verbose", (DL_FUNC) set_verbose);
  R_RegisterCCallable(package, "get_verbose", (DL_FUNC) get_verbose);
  R_RegisterCCallable(package, "set_timeout", (DL_FUNC) set_timeout);
  R_RegisterCCallable(package, "get_timeout", (DL_FUNC) get_timeout);
  R_RegisterCCallable(package, "set_print_sol", (DL_FUNC) set_print_sol);
  R_RegisterCCallable(package, "get_print_sol", (DL_FUNC) get_print_sol);
  R_RegisterCCallable(package, "set_debug", (DL_FUNC) set_debug);
  R_RegisterCCallable(package, "is_debug", (DL_FUNC) is_debug);
  R_RegisterCCallable(package, "set_trace", (DL_FUNC) set_trace);
  R_RegisterCCallable(package, "is_trace", (DL_FUNC) is_trace);
  R_RegisterCCallable(package, "print_debugdump", (DL_FUNC) print_debugdump);
  R_RegisterCCallable(package, "set_anti_degen", (DL_FUNC) set_anti_degen);
  R_RegisterCCallable(package, "get_anti_degen", (DL_FUNC) get_anti_degen);
  R_RegisterCCallable(package, "is_anti_degen", (DL_FUNC) is_anti_degen);
  R_RegisterCCallable(package, "set_presolve", (DL_FUNC) set_presolve);
  R_RegisterCCallable(package, "get_presolve", (DL_FUNC) get_presolve);
  R_RegisterCCallable(package, "get_presolveloops", (DL_FUNC) get_presolveloops);
  R_RegisterCCallable(package, "is_presolve", (DL_FUNC) is_presolve);
  R_RegisterCCallable(package, "get_orig_index", (DL_FUNC) get_orig_index);
  R_RegisterCCallable(package, "get_lp_index", (DL_FUNC) get_lp_index);
  R_RegisterCCallable(package, "set_maxpivot", (DL_FUNC) set_maxpivot);
  R_RegisterCCallable(package, "get_maxpivot", (DL_FUNC) get_maxpivot);
  R_RegisterCCallable(package, "set_obj_bound", (DL_FUNC) set_obj_bound);
  R_RegisterCCallable(package, "get_obj_bound", (DL_FUNC) get_obj_bound);
  R_RegisterCCallable(package, "set_mip_gap", (DL_FUNC) set_mip_gap);
  R_RegisterCCallable(package, "get_mip_gap", (DL_FUNC) get_mip_gap);
  R_RegisterCCallable(package, "set_bb_rule", (DL_FUNC) set_bb_rule);
  R_RegisterCCallable(package, "get_bb_rule", (DL_FUNC) get_bb_rule);
  R_RegisterCCallable(package, "set_var_branch", (DL_FUNC) set_var_branch);
  R_RegisterCCallable(package, "get_var_branch", (DL_FUNC) get_var_branch);
  R_RegisterCCallable(package, "is_infinite", (DL_FUNC) is_infinite);
  R_RegisterCCallable(package, "set_infinite", (DL_FUNC) set_infinite);
  R_RegisterCCallable(package, "get_infinite", (DL_FUNC) get_infinite);
  R_RegisterCCallable(package, "set_epsint", (DL_FUNC) set_epsint);
  R_RegisterCCallable(package, "get_epsint", (DL_FUNC) get_epsint);
  R_RegisterCCallable(package, "set_epsb", (DL_FUNC) set_epsb);
  R_RegisterCCallable(package, "get_epsb", (DL_FUNC) get_epsb);
  R_RegisterCCallable(package, "set_epsd", (DL_FUNC) set_epsd);
  R_RegisterCCallable(package, "get_epsd", (DL_FUNC) get_epsd);
  R_RegisterCCallable(package, "set_epsel", (DL_FUNC) set_epsel);
  R_RegisterCCallable(package, "get_epsel", (DL_FUNC) get_epsel);
  R_RegisterCCallable(package, "set_epslevel", (DL_FUNC) set_epslevel);
  R_RegisterCCallable(package, "set_scaling", (DL_FUNC) set_scaling);
  R_RegisterCCallable(package, "get_scaling", (DL_FUNC) get_scaling);
  R_RegisterCCallable(package, "is_scalemode", (DL_FUNC) is_scalemode);
  R_RegisterCCallable(package, "is_scaletype", (DL_FUNC) is_scaletype);
  R_RegisterCCallable(package, "is_integerscaling", (DL_FUNC) is_integerscaling);
  R_RegisterCCallable(package, "set_scalelimit", (DL_FUNC) set_scalelimit);
  R_RegisterCCallable(package, "get_scalelimit", (DL_FUNC) get_scalelimit);
  R_RegisterCCallable(package, "set_improve", (DL_FUNC) set_improve);
  R_RegisterCCallable(package, "get_improve", (DL_FUNC) get_improve);
  R_RegisterCCallable(package, "set_pivoting", (DL_FUNC) set_pivoting);
  R_RegisterCCallable(package, "get_pivoting", (DL_FUNC) get_pivoting);
  R_RegisterCCallable(package, "set_partialprice", (DL_FUNC) set_partialprice);
  R_RegisterCCallable(package, "get_partialprice", (DL_FUNC) get_partialprice);
  R_RegisterCCallable(package, "set_multiprice", (DL_FUNC) set_multiprice);
  R_RegisterCCallable(package, "get_multiprice", (DL_FUNC) get_multiprice);
  R_RegisterCCallable(package, "is_use_names", (DL_FUNC) is_use_names);
  R_RegisterCCallable(package, "set_use_names", (DL_FUNC) set_use_names);
  R_RegisterCCallable(package, "get_nameindex", (DL_FUNC) get_nameindex);
  R_RegisterCCallable(package, "is_piv_mode", (DL_FUNC) is_piv_mode);
  R_RegisterCCallable(package, "is_piv_rule", (DL_FUNC) is_piv_rule);
  R_RegisterCCallable(package, "set_break_at_first", (DL_FUNC) set_break_at_first);
  R_RegisterCCallable(package, "is_break_at_first", (DL_FUNC) is_break_at_first);
  R_RegisterCCallable(package, "set_bb_floorfirst", (DL_FUNC) set_bb_floorfirst);
  R_RegisterCCallable(package, "get_bb_floorfirst", (DL_FUNC) get_bb_floorfirst);
  R_RegisterCCallable(package, "set_bb_depthlimit", (DL_FUNC) set_bb_depthlimit);
  R_RegisterCCallable(package, "get_bb_depthlimit", (DL_FUNC) get_bb_depthlimit);
  R_RegisterCCallable(package, "set_break_at_value", (DL_FUNC) set_break_at_value);
  R_RegisterCCallable(package, "get_break_at_value", (DL_FUNC) get_break_at_value);
  R_RegisterCCallable(package, "set_negrange", (DL_FUNC) set_negrange);
  R_RegisterCCallable(package, "get_negrange", (DL_FUNC) get_negrange);
  R_RegisterCCallable(package, "set_epsperturb", (DL_FUNC) set_epsperturb);
  R_RegisterCCallable(package, "get_epsperturb", (DL_FUNC) get_epsperturb);
  R_RegisterCCallable(package, "set_epspivot", (DL_FUNC) set_epspivot);
  R_RegisterCCallable(package, "get_epspivot", (DL_FUNC) get_epspivot);
  R_RegisterCCallable(package, "get_max_level", (DL_FUNC) get_max_level);
  R_RegisterCCallable(package, "get_total_nodes", (DL_FUNC) get_total_nodes);
  R_RegisterCCallable(package, "get_total_iter", (DL_FUNC) get_total_iter);
  R_RegisterCCallable(package, "get_objective", (DL_FUNC) get_objective);
  R_RegisterCCallable(package, "get_working_objective", (DL_FUNC) get_working_objective);
  R_RegisterCCallable(package, "get_var_primalresult", (DL_FUNC) get_var_primalresult);
  R_RegisterCCallable(package, "get_var_dualresult", (DL_FUNC) get_var_dualresult);
  R_RegisterCCallable(package, "get_variables", (DL_FUNC) get_variables);
  R_RegisterCCallable(package, "get_ptr_variables", (DL_FUNC) get_ptr_variables);
  R_RegisterCCallable(package, "get_constraints", (DL_FUNC) get_constraints);
  R_RegisterCCallable(package, "get_ptr_constraints", (DL_FUNC) get_ptr_constraints);
  R_RegisterCCallable(package, "get_sensitivity_rhs", (DL_FUNC) get_sensitivity_rhs);
  R_RegisterCCallable(package, "get_ptr_sensitivity_rhs", (DL_FUNC) get_ptr_sensitivity_rhs);
  R_RegisterCCallable(package, "get_sensitivity_obj", (DL_FUNC) get_sensitivity_obj);
  R_RegisterCCallable(package, "get_sensitivity_objex", (DL_FUNC) get_sensitivity_objex);
  R_RegisterCCallable(package, "get_ptr_sensitivity_obj", (DL_FUNC) get_ptr_sensitivity_obj);
  R_RegisterCCallable(package, "get_ptr_sensitivity_objex", (DL_FUNC) get_ptr_sensitivity_objex);
  R_RegisterCCallable(package, "set_solutionlimit", (DL_FUNC) set_solutionlimit);
  R_RegisterCCallable(package, "get_solutionlimit", (DL_FUNC) get_solutionlimit);
  R_RegisterCCallable(package, "get_solutioncount", (DL_FUNC) get_solutioncount);
  R_RegisterCCallable(package, "get_Norig_rows", (DL_FUNC) get_Norig_rows);
  R_RegisterCCallable(package, "get_Nrows", (DL_FUNC) get_Nrows);
  /*R_RegisterCCallable(package, "get_Lrows", (DL_FUNC) get_Lrows);*/
  R_RegisterCCallable(package, "get_Norig_columns", (DL_FUNC) get_Norig_columns);
  R_RegisterCCallable(package, "get_Ncolumns", (DL_FUNC) get_Ncolumns);
}

