tcplLiteInit <- function () {
  # Create initial flat files with appropriate fields
  db = getOption("TCPL_DB")
  all_files <- list('assay_source'= c('asid', 'assay_source_name', 'assay_source_long_name', 'assay_source_desc'),
                    'assay'= c('aid', 'asid', 'assay_name', 'assay_desc', 'timepoint_hr', 'organism_id', 'organism', 'tissue', 'cell_format', 'cell_free_component_source', 'cell_short_name', 'cell_growth_mode', 'assay_footprint', 'assay_format_type', 'assay_format_type_sub', 'content_readout_type', 'dilution_solvent', 'dilution_solvent_percent_max'),
                    'assay_component'= c('acid', 'aid', 'assay_component_name', 'assay_component_desc', 'assay_component_target_desc', 'parameter_readout_type', 'assay_design_type', 'assay_design_type_sub', 'biological_process_target', 'detection_technology_type', 'detection_technology_type_sub', 'detection_technology', 'signal_direction_type', 'key_assay_reagent_type', 'key_assay_reagent', 'technological_target_type', 'technological_target_type_sub'),
                    'assay_component_endpoint'=c('aeid', 'acid', 'assay_component_endpoint_name', 'export_ready', 'internal_ready', 'assay_component_endpoint_desc', 'assay_function_type', 'normalized_data_type', 'analysis_direction', 'burst_assay', 'key_positive_control', 'signal_direction', 'intended_target_type', 'intended_target_type_sub', 'intended_target_family', 'intended_target_family_sub', 'fit_all'),
                    'assay_component_map'= c('acid', 'acsn'),
                    'sample'= c('spid', 'chid', 'stkc', 'stkc_unit', 'tested_conc_unit', 'spid_legacy'),
                    'chemical_library'= c('chid', 'clib'),
                    
                    'mc0'= c('m0id', 'acid', 'spid', 'apid', 'rowi', 'coli', 'wllt', 'wllq', 'conc', 'rval', 'srcf', 'created_date', 'modified_date', 'modified_by'),
                    'mc1' = c('m1id', 'm0id', 'acid', 'cndx', 'repi', 'created_date', 'modified_date', 'modified_by'),
                    'mc2' = c('m2id', 'm0id', 'acid', 'm1id', 'cval', 'created_date', 'modified_date', 'modified_by'),
                    
                    'mc3' = c('m3id', 'aeid', 'm0id', 'acid', 'm1id', 'm2id', 'bval', 'pval', 'logc', 'resp', 'created_date', 'modified_date', 'modified_by'),
                    'mc4' = c('m4id', 'aeid', 'spid', 'bmad', 'resp_max', 'resp_min', 'max_mean', 'max_mean_conc', 'max_med', 'max_med_conc', 'logc_max', 'logc_min', 'cnst', 'hill', 'hcov', 'gnls', 'gcov', 'cnst_er', 'cnst_aic', 'cnst_rmse', 'cnst_prob', 'hill_tp', 'hill_tp_sd', 'hill_ga', 'hill_ga_sd', 'hill_gw', 'hill_gw_sd', 'hill_er', 'hill_er_sd', 'hill_aic', 'hill_rmse', 'hill_prob', 'gnls_tp', 'gnls_tp_sd', 'gnls_ga', 'gnls_ga_sd', 'gnls_gw', 'gnls_gw_sd', 'gnls_la', 'gnls_la_sd', 'gnls_lw', 'gnls_lw_sd', 'gnls_er', 'gnls_er_sd', 'gnls_aic', 'gnls_rmse', 'gnls_prob', 'nconc', 'npts', 'nrep', 'nmed_gtbl', 'tmpi', 'created_date', 'modified_date', 'modified_by'),
                    'mc4_agg' = c('aeid', 'm0id', 'm1id', 'm2id', 'm3id', 'm4id'),
                    'mc5' = c('m5id', 'm4id', 'aeid', 'modl', 'hitc', 'fitc', 'coff', 'actp', 'modl_er', 'modl_tp', 'modl_ga', 'modl_gw', 'modl_la', 'modl_lw', 'modl_prob', 'modl_rmse', 'modl_acc', 'modl_acb', 'modl_ac10', 'created_date', 'modified_date', 'modified_by'),
                    'mc6' = c('m6id', 'm5id', 'm4id', 'aeid', 'mc6_mthd_id', 'flag', 'fval', 'fval_unit', 'created_date', 'modified_date', 'modified_by'),
                    
                    'mc2_acid' = c('mc2_mthd_id', 'acid', 'exec_ordr', 'created_date', 'modified_date', 'modified_by'),
                    'mc3_aeid' = c('mc3_mthd_id', 'aeid', 'exec_ordr', 'created_date', 'modified_date', 'modified_by'),
                    'mc4_aeid' = c('mc4_mthd_id', 'aeid', 'created_date', 'modified_date', 'modified_by'),
                    'mc5_aeid' = c('aeid', 'mc5_mthd_id', 'created_date', 'modified_date', 'modified_by'),
                    'mc6_aeid' = c('aeid', 'mc6_mthd_id', 'created_date', 'modified_date', 'modified_by'),
                    
                    'sc0' = c('s0id', 'acid', 'spid', 'apid', 'rowi', 'coli', 'wllt', 'wllq', 'conc', 'rval', 'srcf', 'created_date', 'modified_date', 'modified_by'),
                    'sc1' = c('s1id', 's0id', 'acid', 'aeid', 'logc', 'bval', 'pval', 'resp', 'created_date', 'modified_date', 'modified_by'),
                    'sc2' = c('s2id', 'aeid', 'spid', 'bmad', 'max_med', 'coff', 'hitc', 'tmpi', 'created_date', 'modified_date', 'modified_by'),
                    'sc2_agg' = c('aeid', 's0id', 's1id', 's2id'),
                    'sc1_aeid' = c('sc1_mthd_id', 'aeid', 'exec_ordr', 'created_date', 'modified_date', 'modified_by'),
                    'sc2_aeid' = c('sc2_mthd_id', 'aeid', 'created_date', 'modified_date', 'modified_by')
                    )
  for (k in names(all_files)) {
    v = all_files[[k]]
    fpath <- paste(db, k, sep='/')
    fpath <- paste(fpath, 'csv', sep='.')
    if (!file.exists(fpath)) {
      df <- data.frame(matrix(ncol=length(v), nrow=0))
      colnames(df) <- v
      print(sprintf("Writing local %s table for future referencing.", k))
      write.table(df, file=fpath, append=F, row.names=F, sep=',', col.names=T)
    }
  }
  
  methods = c("mc2_methods", "mc3_methods","mc4_methods", "mc5_methods","mc5_fit_categories", "mc6_methods", "sc1_methods", "sc2_methods", "chemical")
  
  for (m in methods) {
    tcpl_path <- find.package('tcpl')
    tcpl_fpath <- paste(tcpl_path,'csv',sep='/')
    tcpl_fpath <- paste(tcpl_fpath, m, sep='/')
    tcpl_fpath <- paste(tcpl_fpath, 'csv', sep='.')
    fpath <- paste(db, m, sep='/')
    fpath <- paste(fpath, 'csv', sep='.')
    if (!file.exists(fpath)) {
      print(sprintf("Writing local %s table for future referencing.", m))
      file.copy(tcpl_fpath, db)
    }
    
    
  }
    
  
  
  

  
  
  
}