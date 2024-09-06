#' A subset of ToxCast data showing changes in transcription factor activity for multiple targets.
#'
#' The example dataset is used to illustrate how the user can pipeline single-concentration
#' data from chemical screening using tcpl.
#'
#' @format A data frame with 320 rows and 10 variables:
#' \describe{
#'
#'  \item{spid}{sample ID}
#'   \item{apid}{assay plate ID}
#'    \item{rowi}{well-plate row number (N/A)}
#'     \item{coli}{well-plate column number (N/A)}
#'      \item{wllt}{well type (N/A)}
#'       \item{wllq}{well quality (N/A)}
#'        \item{conc}{concentration in micromolar}
#'         \item{rval}{raw assay component readout value}
#'          \item{srcf}{source file containing the data}
#'           \item{acsn}{assay component source name}
#' }
#' @source ToxCast database
"scdat"
#' A subset of ToxCast data showing changes in the activity of the intracellular estrogen receptor.
#'
#' The example dataset is used to illustrate how the user can pipeline multiple-concentration
#' data from chemical screening using tcpl.
#'
#' @format A data frame with 14183 rows and 10 variables:
#' \describe{
#'  \item{spid}{sample ID}
#'   \item{apid}{assay plate ID}
#'    \item{rowi}{well-plate row number}
#'     \item{coli}{well-plate column number}
#'      \item{wllt}{well type}
#'       \item{wllq}{well quality}
#'        \item{conc}{concentration in micromolar}
#'         \item{rval}{raw assay component readout value}
#'          \item{srcf}{source file containing the data}
#'           \item{acsn}{assay component source name}
#' }
#' @source ToxCast database
"mcdat"


#' List with multi-concentration data for the vignette
#'
#' This dataset is a list with 6 data.tables (mc0,mc1,mc2,mc3,mc4,mc5).
#'
#' @format
#' \enumerate{
#' \item \strong{mc0} A data frame with 78 rows and 18 columns containing
#'                    level 0 formatted raw data.
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{m0id}{Level 0 (mc0) ID}
#'   \item{apid}{Assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{wllq}{Well Quality (0 or 1)}
#'   \item{conc}{Concentration in micromolar}
#'   \item{rval}{Raw assay component readout value}
#'   \item{srcf}{Source file containing the raw data}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#' \item \strong{mc1} A data frame with 78 rows and 21 columns containing
#'                    level 1 replicate and concentration level indicated data.
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{m0id}{Level 0 (mc0) ID}
#'   \item{m1id}{Level 1 (mc1) ID}
#'   \item{apid}{Assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{wllq}{Well Quality (0 or 1)}
#'   \item{conc}{Concentration in micromolar}
#'   \item{rval}{Raw assay component readout value}
#'   \item{cndx}{Concentration index defined by ranking the unique concentrations, with the lowest concentration starting at 1.}
#'   \item{repi}{Temporary replicate ID is defined, the data are scanned from top to bottom and increment the replicate index every time a replicate ID is duplicated}
#'   \item{srcf}{Source file containing the raw data}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#'
#' \item \strong{mc2} A data frame with 78 rows and 20 columns containing
#'                    level 2 assay component-specific corrections.
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{m0id}{Level 0 (mc0) ID}
#'   \item{m1id}{Level 1 (mc1) ID}
#'   \item{m2id}{Level 2 (mc2) ID}
#'   \item{apid}{Assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{conc}{Concentration in micromolar}
#'   \item{cval}{Corrected Value}
#'   \item{cndx}{Concentration index defined by ranking the unique concentrations, with the lowest concentration starting at 1.}
#'   \item{repi}{Temporary replicate ID is defined, the data are scanned from top to bottom and increment the replicate index every time a replicate ID is duplicated}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#'
#' \item \strong{mc3} A data frame with 78 rows and 22 columns containing
#'                    level 3 assay endpoint normalized data.
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{Assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{m0id}{Level 0 (mc0) ID}
#'   \item{m1id}{Level 1 (mc1) ID}
#'   \item{m2id}{Level 2 (mc2) ID}
#'   \item{m3id}{Level 3 (mc3) ID}
#'   \item{logc}{Log base 10 concentration}
#'   \item{resp}{Normalized response value}
#'   \item{cndx}{Concentration index defined by ranking the unique concentrations, with the lowest concentration starting at 1.}
#'   \item{wllt}{Well Type}
#'   \item{apid}{Assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{repi}{Temporary replicate ID is defined, the data are scanned from top to bottom and increment the replicate index every time a replicate ID is duplicated}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#'
#' \item \strong{mc4} A data frame with 5 rows and 149 columns containing
#'                    level 4 concentration-response fitting data (all fits).
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{Assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{m4id}{Level 4 (mc4) ID}
#'   \item{bmad}{The median absolute deviation of all treatment wells (default option) or blank wells}
#'   \item{resp_max}{Maximum observed response}
#'   \item{resp_min}{Minimum observed response}
#'   \item{max_mean}{Maximum mean response}
#'   \item{max_mean_conc}{Concentration of the maximum mean response}
#'   \item{max_med}{Maximum median response}
#'   \item{max_med_conc}{Concentration of the maximum median response}
#'   \item{logc_max}{Maximum concentration on the log scale}
#'   \item{logc_min}{Minimum concentration on the log scale}
#'   \item{nconc}{The total number of concentration groups}
#'   \item{npts}{Total number of observed responses (i.e. data points in the concentration series) }
#'   \item{nrep}{Number of replicates in concentration groups}
#'   \item{nmed_gtbl}{The number of median responses greater than 3BMAD}
#'   \item{cnst_success}{Success indicator for the Constant model; 1 if the optimization was successful, otherwise 0}
#'   \item{cnst_aic}{Akaike Information Criteria (AIC) for the Constant model}
#'   \item{cnst_rme}{Root mean square error for the Constant model}
#'   \item{cnst_er}{Error term for the Constant model}
#'   \item{hill_success}{Success indicator for the Hill model; 1 if the optimization was successful, otherwise 0}
#'   \item{hill_aic}{Akaike Information Criteria (AIC) for the Hill model}
#'   \item{hill_cov}{Success indicator for the Hill model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{hill_rme}{Root mean square erro for the Hill model}
#'   \item{hill_tp}{The top parameter indicating the maximal estimated response}
#'   \item{hill_ga}{The gain parameter for the Hill model, gain AC50}
#'   \item{hill_p}{The power parameter for the Hill model}
#'   \item{hill_er}{Error term for the Hill model}
#'   \item{hill_tp_sd}{Standard deviation of the Hill model top parameter}
#'   \item{hill_ga_sd}{Standard deviation of the Hill model gain parameter}
#'   \item{hill_p_sd}{Standard deviation of the Hill model power parameter}
#'   \item{hill_er_sd}{Standard deviation of the Hill model error term}
#'   \item{hill_top}{The maximal response on the resulting Hill model fit}
#'   \item{hill_ac50}{Concentration at 50\% of the maximal response on the Hill model fit}
#'   \item{gnls_success}{Success indicator for the Gain-loss model; 1 if the optimization was successful, otherwise 0}
#'   \item{gnls_aic}{Akaike Information Criteria (AIC) for the Gain-loss model}
#'   \item{gnls_cov}{Success indicator for the Gain-loss model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{gnls_rme}{Root mean square erro for the Gain-loss model}
#'   \item{gnls_tp}{The top parameter indicating the maximal estimated response}
#'   \item{gnls_ga}{The gain parameter for the Gain-loss model, gain AC50}
#'   \item{gnls_p}{The gain power parameter for the Gain-loss model}
#'   \item{gnls_la}{The loss parameter for the Gain-loss model, loss AC50}
#'   \item{gnls_q}{The loss power parameter for the Gain-loss model}
#'   \item{gnls_er}{Error term for the Gain-loss model}
#'   \item{gnls_tp_sd}{Standard deviation of the Gain-loss model top parameter}
#'   \item{gnls_ga_sd}{Standard deviation of the Gain-loss model gain parameter}
#'   \item{gnls_p_sd}{Standard deviation of the Gain-loss model gain power parameter}
#'   \item{gnls_la_sd}{Standard deviation of the Gain-loss model loss parameter}
#'   \item{gnls_q_sd}{Standard deviation of the Gain-loss model loss power parameter}
#'   \item{gnls_er_sd}{Standard deviation of the Gain-loss model error term}
#'   \item{gnls_top}{The maximal response on the resulting Gain-loss model fit}
#'   \item{gnls_ac50}{Concentration at 50\% of the maximal response on the Gain-loss model fit, gain AC50}
#'   \item{gnls_ac50_loss}{Concentration at 50\% of the maximal response on the Gain-loss model fit, loss AC50}
#'   \item{poly1_success}{Success indicator for the Polynomial 1 model; 1 if the optimization was successful, otherwise 0}
#'   \item{poly1_aic}{Akaike Information Criteria (AIC) for the Polynomial 1 model}
#'   \item{poly1_cov}{Success indicator for the Polynomial 1 model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{poly1_rme}{Root mean square erro for the Polynomial 1 model}
#'   \item{poly1_a}{The y-scale parameter for the Polynomial 1 model}
#'   \item{poly1_er}{Error term for the Polynomial 1 model}
#'   \item{poly1_a_sd}{Standard deviation of the Polynomial 1 model y-scale parameter}
#'   \item{poly1_er_sd}{Standard deviation of the Polynomial 1 model error term}
#'   \item{poly1_top}{The maximal response on the resulting Polynomial 1 model fit}
#'   \item{poly1_ac50}{Concentration at 50\% of the maximal response on the Polynomial 1 model fit}
#'   \item{poly2_success}{Success indicator for the Polynomial 2 model; 1 if the optimization was successful, otherwise 0}
#'   \item{poly2_aic}{Akaike Information Criteria (AIC) for the Polynomial 2 model}
#'   \item{poly2_cov}{Success indicator for the Polynomial 2 model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{poly2_rme}{Root mean square erro for the Polynomial 2 model}
#'   \item{poly2_a}{The y-scale parameter for the Polynomial 2 model}
#'   \item{poly2_b}{The x-scale parameter for the Polynomial 2 model}
#'   \item{poly2_er}{Error term for the Polynomial 2 model}
#'   \item{poly2_a_sd}{Standard deviation of the Polynomial 2 model y-scale parameter}
#'   \item{poly2_b_sd}{Standard deviation of the Polynomial 2 model x-scale parameter}
#'   \item{poly2_er_sd}{Standard deviation of the Polynomial 2 model error term}
#'   \item{poly2_top}{The maximal response on the resulting Polynomial 2 model fit}
#'   \item{poly2_ac50}{Concentration at 50\% of the maximal response on the Polynomial 2 model fit}
#'   \item{pow_success}{Success indicator for the Power model; 1 if the optimization was successful, otherwise 0}
#'   \item{pow_aic}{Akaike Information Criteria (AIC) for the Power model}
#'   \item{pow_cov}{Success indicator for the Power model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{pow_rme}{Root mean square erro for the Power model}
#'   \item{pow_a}{The y-scale parameter for the Power model}
#'   \item{pow_p}{The power parameter for the Power model}
#'   \item{pow_er}{Error term for the Power model}
#'   \item{pow_a_sd}{Standard deviation of the Power model y-scale parameter}
#'   \item{pow_p_sd}{Standard deviation of the Power model power parameter}
#'   \item{pow_er_sd}{Standard deviation of the Power model error term}
#'   \item{pow_top}{The maximal response on the resulting Power model fit}
#'   \item{pow_ac50}{Concentration at 50\% of the maximal response on the Power model fit}
#'   \item{exp2_success}{Success indicator for the Exponential 2 model; 1 if the optimization was successful, otherwise 0}
#'   \item{exp2_aic}{Akaike Information Criteria (AIC) for the Exponential 2 model}
#'   \item{exp2_cov}{Success indicator for the Exponential 2 model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{exp2_rme}{Root mean square erro for the Exponential 2 model}
#'   \item{exp2_a}{The y-scale parameter for the Exponential 2 model}
#'   \item{exp2_b}{The x-scale parameter for the Exponential 2 model}
#'   \item{exp2_er}{Error term for the Exponential 2 model}
#'   \item{exp2_a_sd}{Standard deviation of the Exponential 2 model y-scale parameter}
#'   \item{exp2_b_sd}{Standard deviation of the Exponential 2 model x-scale parameter}
#'   \item{exp2_er_sd}{Standard deviation of the Exponential 2 model error term}
#'   \item{exp2_top}{The maximal response on the resulting Exponential 2 model fit}
#'   \item{exp2_ac50}{Concentration at 50\% of the maximal response on the Exponential 2 model fit}
#'   \item{exp3_success}{Success indicator for the Exponential 3 model; 1 if the optimization was successful, otherwise 0}
#'   \item{exp3_aic}{Akaike Information Criteria (AIC) for the Exponential 3 model}
#'   \item{exp3_cov}{Success indicator for the Exponential 3 model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{exp3_rme}{Root mean square erro for the Exponential 3 model}
#'   \item{exp3_a}{The y-scale parameter for the Exponential 3 model}
#'   \item{exp3_b}{The x-scale parameter for the Exponential 3 model}
#'   \item{exp3_p}{The power parameter for the Exponential 3 model}
#'   \item{exp3_er}{Error term for the Exponential 3 model}
#'   \item{exp3_a_sd}{Standard deviation of the Exponential 3 model y-scale parameter}
#'   \item{exp3_b_sd}{Standard deviation of the Exponential 3 model x-scale parameter}
#'   \item{exp3_p_sd}{Standard deviation of the Exponential 3 model power parameter}
#'   \item{exp3_er_sd}{Standard deviation of the Exponential 3 model error term}
#'   \item{exp3_top}{The maximal response on the resulting Exponential 3 model fit}
#'   \item{exp3_ac50}{Concentration at 50\% of the maximal response on the Exponential 3 model fit}
#'   \item{exp4_success}{Success indicator for the Exponential 4 model; 1 if the optimization was successful, otherwise 0}
#'   \item{exp4_aic}{Akaike Information Criteria (AIC) for the Exponential 4 model}
#'   \item{exp4_cov}{Success indicator for the Exponential 4 model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{exp4_rme}{Root mean square erro for the Exponential 4 model}
#'   \item{exp4_tp}{The top parameter indicating the maximal estimated response}
#'   \item{exp4_ga}{The gain parameter for the Exponential 4 model, gain AC50}
#'   \item{exp4_er}{Error term for the Exponential 4 model}
#'   \item{exp4_tp_sd}{Standard deviation of the Exponential 4 model top parameter}
#'   \item{exp4_ga_sd}{Standard deviation of the Exponential 4 model gain parameter}
#'   \item{exp4_er_sd}{Standard deviation of the Exponential 4 model error term}
#'   \item{exp4_top}{The maximal response on the resulting Exponential 4 model fit}
#'   \item{exp4_ac50}{Concentration at 50\% of the maximal response on the Exponential 4 model fit}
#'   \item{exp5_success}{Success indicator for the Exponential 5 model; 1 if the optimization was successful, otherwise 0}
#'   \item{exp5_aic}{Akaike Information Criteria (AIC) for the Exponential 5 model}
#'   \item{exp5_cov}{Success indicator for the Exponential 5 model covariance calculation; 1 if the Hessian matrix inversion is successful, otherwise 0}
#'   \item{exp5_rme}{Root mean square erro for the Exponential 5 model}
#'   \item{exp5_tp}{The top parameter indicating the maximal estimated response}
#'   \item{exp5_ga}{The gain parameter for the Exponential 5 model, gain AC50}
#'   \item{exp5_p}{The power parameter for the Exponential 5 model}
#'   \item{exp5_er}{Error term for the Exponential 5 model}
#'   \item{exp5_tp_sd}{Standard deviation of the Exponential 5 model top parameter}
#'   \item{exp5_ga_sd}{Standard deviation of the Exponential 5 model gain parameter}
#'   \item{exp5_p_sd}{Standard deviation of the Exponential 5 model power parameter}
#'   \item{exp5_er_sd}{Standard deviation of the Exponential 5 model error term}
#'   \item{exp5_top}{The maximal response on the resulting Exponential 5 model fit}
#'   \item{exp5_ac50}{Concentration at 50\% of the maximal response on the Exponential 5 model fit}
#'   \item{all_onesd}{Standard deviation of the baseline response for all models}
#'   \item{all_bmed}{Median noise estimation of the baseline response for all models}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#'
#' \item \strong{mc5} A data frame with 5 rows and 54 columns containing
#'                    level 5 best curve-fit and hitcall data.
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{Assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{m5id}{Level 5 (mc5) ID}
#'   \item{m4id}{Level 4 (mc4) ID}
#'   \item{bmad}{The median absolute deviation of all treatment wells (default option) or blank wells }
#'   \item{resp_max}{Maximum observed response}
#'   \item{resp_min}{Minimum observed response}
#'   \item{max_mean}{Maximum mean response}
#'   \item{max_mean_conc}{Concentration of the maximum mean response}
#'   \item{max_med}{Maximum median response}
#'   \item{max_med_conc}{Concentration of the maximum median response}
#'   \item{logc_max}{Maximum concentration on the log scale}
#'   \item{logc_min}{Minimum concentration on the log scale}
#'   \item{nconc}{The total number of concentration groups}
#'   \item{npts}{Total number of observed responses (i.e. data points in the concentration series)}
#'   \item{nrep}{Number of replicates in concentration groups}
#'   \item{nmed_gtbl}{The number of median responses greater than 3BMAD}
#'   \item{hitc}{Hitcall}
#'   \item{modl}{Best model fit from tcplFit2 curve-fitting}
#'   \item{fitc}{Fit category}
#'   \item{coff}{Cutoff}
#'   \item{top_over_cutoff}{Ratio of the top of the best model fit curve and the cutoff}
#'   \item{rmse}{Root mean squared error}
#'   \item{a}{The y-scale parameter for poly1, poly2, pow, exp2, or exp3 model}
#'   \item{er}{Error term}
#'   \item{bmr}{Benchmark response}
#'   \item{bmdl}{Lower 95\% confidence bound on the benchmark dose/concentration estimate}
#'   \item{caikwt}{Akaike Information Criteria weight of constant model relative to the best model fit}
#'   \item{mll}{Maximum log-likelihood of the best model fit}
#'   \item{hitcall}{Continuous hitcall}
#'   \item{ac50}{Concentration where 50\% of the maximal response occurs - if 'modl' is the Hill or Gain-loss model this is for the "gain" side of the response}
#'   \item{top}{The maximal response on the best model curve fit - i.e. top of the curve fit}
#'   \item{ac5}{Concentration where 5\% of the maximal response occurs}
#'   \item{ac10}{Concentration where 10\% of the maximal response occurs}
#'   \item{ac20}{Concentration where 20\% of the maximal response occurs}
#'   \item{acc}{Concentration where the efficacy cutoff response occurs}
#'   \item{ac1sd}{Concentration where one standard deviation of the background response occurs}
#'   \item{bmd}{Benchmark response/concentration estimate - concentration where the benchmark response occurs}
#'   \item{bmdu}{Upper 95\% confidence bound on the benchmark dose/concentration estimate}
#'   \item{tp}{The top curve parameter for the exp4, exp5, hill, or gnls model}
#'   \item{ga}{The gain parameter for the hill or gnls model - gain AC50}
#'   \item{p}{The power parameter for the pow, exp3, exp5, gnls, or hill model - for gnls this is the gain power parameter}
#'   \item{q}{The loss power parameter for the gnls model}
#'   \item{la}{The loss parameter for the gnls model, loss AC50}
#'   \item{ac50_loss}{Concentration where 50\% of the maximal response occurs - if 'modl' is the Hill or Gain-loss model this is for the "loss" side of the response}
#'   \item{b}{The x-scale parameter for poly2, exp2, or exp3 model}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#' }
"mc_vignette"

#' List with single-concentration data for the vignette
#'
#' This dataset is a list with 3 data.tables (sc0,sc1,sc2).
#'
#' @format
#' \enumerate{
#' \item \strong{sc0} A data frame with 10 rows and 18 columns containing
#'                    level 0 formatted raw data.
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{s0id}{Level 0 (sc0) ID}
#'   \item{apid}{Assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{wllq}{Well Quality (0 or 1)}
#'   \item{conc}{Concentration in micromolar}
#'   \item{rval}{Raw assay component readout value}
#'   \item{srcf}{Source file containing the raw data}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#'
#' \item \strong{sc1} A data frame with 10 rows and 20 columns containing
#'                    level 1 normalized data.
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{Assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{s0id}{Level 0 (sc0) ID}
#'   \item{s1id}{Level 1 (sc1) ID}
#'   \item{apid}{Assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{logc}{Log base 10 concentration}
#'   \item{resp}{Normalized response value}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#'
#' \item \strong{sc2} A data frame with 10 rows and 15 columns containing
#'                    level 2 efficacy/hit designation data.
#' \describe{
#'   \item{spid}{Sample ID}
#'   \item{chid}{Unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{Chemical name}
#'   \item{dsstox_substance_id}{Chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{Assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{s2id}{Level 2 (sc2) ID}
#'   \item{bmad}{The median absolute deviation of all treatment wells (default option) or blank wells }
#'   \item{max_med}{Maximum median response}
#'   \item{hitc}{Hitcall}
#'   \item{coff}{Cutoff}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#' }
"sc_vignette"


#' Short descriptions of fields for different tables are stored in a data dictionary.
#' @format A data frame with 44 rows and 3 variables:
#' \describe{
#'  \item{invitrodb_table}{Table of the data dictionary}
#'   \item{invitrodb_field}{Field of the data dictionary}
#'    \item{description}{Description}
#' }
#' @source ToxCast database
"invitrodb_dd"


#' Lists of column names returned from tcplLoadData invitrodb v4.1 (same as CCTE
#' Bioactivity API version).
#' @format A list with 12 items:
#' \describe{
#'   \item{mc0}{Column names returned requesting mc lvl 0 data}
#'   \item{mc1}{Column names returned requesting mc lvl 1 data}
#'   \item{mc2}{Column names returned requesting mc lvl 2 data}
#'   \item{mc3}{Column names returned requesting mc lvl 3 data}
#'   \item{mc4}{Column names returned requesting mc lvl 4 data}
#'   \item{mc5}{Column names returned requesting mc lvl 5 data}
#'   \item{mc6}{Column names returned requesting mc lvl 6 data}
#'   \item{mcagg}{Column names returned requesting mc lvl "agg" data}
#'   \item{sc0}{Column names returned requesting sc lvl 0 data}
#'   \item{sc1}{Column names returned requesting sc lvl 1 data}
#'   \item{sc2}{Column names returned requesting sc lvl 2 data}
#'   \item{scagg}{Column names returned requesting sc lvl "agg" data}
#' }
#' @source ToxCast database
"load_data_columns"


#' List of lists containing queries sent to tcplQuery associated with each test
#' case. Each list also contains the associated ids with each case. Only meant
#' to be used with automated testing with mocking for mc data.
#' @format A list with 30 items:
#' \describe{
#'   \item{tcplConfQuery}{Data table with 1 row and 2 columns used for each test
#'   case for establishing connection using tcplConf. This data table mocks the
#'   response one would get from connecting with invitrodb.}
#'   \item{mc0_by_m0id}{List containing the queries used for loading mc0 data by
#'   m0id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'm0id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc0_by_acid}{List containing the queries used for loading mc0 data by
#'   acid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'acid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc1_by_m1id}{List containing the queries used for loading mc1 data by
#'   m1id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'm1id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc1_by_acid}{List containing the queries used for loading mc1 data by
#'   acid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'acid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc2_by_m2id}{List containing the queries used for loading mc2 data by
#'   m2id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'm2id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc2_by_acid}{List containing the queries used for loading mc2 data by
#'   acid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'acid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc3_by_m3id}{List containing the queries used for loading mc3 data by
#'   m3id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'm3id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc3_by_aeid}{List containing the queries used for loading mc3 data by
#'   aeid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'aeid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc4_by_m4id}{List containing the queries used for loading mc4 data by
#'   m4id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'm4id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc4_by_aeid}{List containing the queries used for loading mc4 data by
#'   aeid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'aeid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc5_by_m5id}{List containing the queries used for loading mc5 data by
#'   m5id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'm5id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc5_by_aeid}{List containing the queries used for loading mc5 data by
#'   aeid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'aeid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc6_by_m6id}{List containing the queries used for loading mc6 data by
#'   m6id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'm6id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc6_by_aeid}{List containing the queries used for loading mc6 data by
#'   aeid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'aeid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc7_by_m7id}{List containing the queries used for loading mc7 data by
#'   m7id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'm7id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mc7_by_aeid}{List containing the queries used for loading mc7 data by
#'   aeid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'aeid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{mcagg_by_aeid}{List containing the queries used for loading mc 'agg'
#'   data by aeid via tcplLoadData. Each query has an associated data table 
#'   response for mocking an actual connection. Contains one 'aeid' labeled item
#'   storing the id used to load the data, for use in tests.}
#'   \item{plot_single_m4id}{List containing the queries used for loading and
#'   plotting mc data by m4id via tcplPlot. Each query has an associated data
#'   table response for mocking an actual connection. Contains one 'm4id'
#'   labeled item storing the id used to load the data, for use in tests.}
#'   \item{plot_multiple_m4id}{List containing the queries used for loading and
#'   plotting mc data by multiple m4ids via tcplPlot. Each query has an
#'   associated data table response for mocking an actual connection. Contains
#'   one 'm4id' labeled item storing the ids used to load the data, for use in
#'   tests.}
#'   \item{plot_single_aeid}{List containing the queries used for loading and
#'   plotting mc data by aeid via tcplPlot. Each query has an associated data
#'   table response for mocking an actual connection. Contains one 'aeid'
#'   labeled item storing the id used to load the data, for use in tests.}
#'   \item{plot_multiple_aeid}{List containing the queries used for loading and
#'   plotting mc data by multiple aeids via tcplPlot. Each query has an
#'   associated data table response for mocking an actual connection. Contains
#'   one 'aeid' labeled item storing the ids used to load the data, for use in
#'   tests.}
#'   \item{plot_single_spid}{List containing the queries used for loading and
#'   plotting mc data by spid/aeid via tcplPlot. Each query has an associated
#'   data table response for mocking an actual connection. Contains 'spid' and
#'   'aeid' labeled items storing the ids used to load the data, for use in
#'   tests.}
#'   \item{plot_multiple_spid}{List containing the queries used for loading and
#'   plotting mc data by multiple spids/aeid via tcplPlot. Each query has an
#'   associated data table response for mocking an actual connection. Contains
#'   'spid' and 'aeid' labeled items storing the ids used to load the data, for
#'   use in tests.}
#'   \item{plot_single_m4id_compare}{List containing the queries used for 
#'   loading and plotting compared mc data by m4id via tcplPlot. Each query has
#'   an associated data table response for mocking an actual connection.
#'   Contains 'm4id' and 'compare.m4id' labeled items storing the ids used to
#'   load the data, for use in tests.}
#'   \item{plot_multiple_m4id_compare}{List containing the queries used for loading and
#'   plotting compared mc data by multiple m4ids via tcplPlot. Each query has an
#'   associated data table response for mocking an actual connection. Contains 
#'   'm4id' and 'compare.m4id' labeled items storing the ids used to load the 
#'   data, for use in tests.}
#'   \item{plot_single_aeid_compare}{List containing the queries used for
#'   loading and plotting compared mc data by aeid via tcplPlot. Each query has
#'   an associated data table response for mocking an actual connection.
#'   Contains 'aeid' and 'compare.aeid' labeled items storing the ids used to
#'   load the data, for use in tests.}
#'   \item{plot_multiple_aeid_compare}{List containing the queries used for
#'   loading and plotting compared mc data by multiple aeids via tcplPlot. Each
#'   query has an associated data table response for mocking an actual
#'   connection. Contains 'aeid' and 'compare.aeid' labeled items storing the
#'   ids used to load the data, for use in tests.}
#'   \item{plot_single_spid_compare}{List containing the queries used for
#'   loading and plotting compared mc data by spid/aeid via tcplPlot. Each query
#'   has an associated data table response for mocking an actual connection.
#'   Contains 'spid', 'compare.spid', 'aeid', and 'compare.aeid' labeled items
#'   storing the ids used to load the data, for use in tests.}
#'   \item{plot_multiple_spid_compare}{List containing the queries used for
#'   loading and plotting compared mc data by multiple spids/aeid via tcplPlot.
#'   Each query has an associated data table response for mocking an actual
#'   connection. Contains 'spid', 'compare.spid', 'aeid', and 'compare.aeid'
#'   labeled items storing the ids used to load the data, for use in tests.}
#' }
#' @source ToxCast database
"mc_test"


#' List of lists containing queries sent to tcplQuery associated with each test
#' case. Each list also contains the associated ids with each case. Only meant
#' to be used with automated testing with mocking for sc data.
#' @format A list with 20 items:
#' \describe{
#'   \item{tcplConfQuery}{Data table with 1 row and 2 columns used for each test
#'   case for establishing connection using tcplConf. This data table mocks the
#'   response one would get from connecting with invitrodb.}
#'   \item{sc0_by_s0id}{List containing the queries used for loading sc0 data by
#'   s0id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 's0id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{sc0_by_acid}{List containing the queries used for loading sc0 data by
#'   acid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'acid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{sc1_by_s1id}{List containing the queries used for loading sc1 data by
#'   s1id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 's1id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{sc1_by_acid}{List containing the queries used for loading sc1 data by
#'   acid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'acid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{sc2_by_s2id}{List containing the queries used for loading sc2 data by
#'   s2id via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 's2id' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{sc2_by_aeid}{List containing the queries used for loading sc2 data by
#'   aeid via tcplLoadData. Each query has an associated data table response for
#'   mocking an actual connection. Contains one 'aeid' labeled item storing the
#'   id used to load the data, for use in tests.}
#'   \item{scagg_by_aeid}{List containing the queries used for loading sc 'agg'
#'   data by aeid via tcplLoadData. Each query has an associated data table 
#'   response for mocking an actual connection. Contains one 'aeid' labeled item
#'   storing the id used to load the data, for use in tests.}
#'   \item{plot_single_s2id}{List containing the queries used for loading and
#'   plotting sc data by s2id via tcplPlot. Each query has an associated data
#'   table response for mocking an actual connection. Contains one 's2id'
#'   labeled item storing the id used to load the data, for use in tests.}
#'   \item{plot_multiple_s2id}{List containing the queries used for loading and
#'   plotting sc data by multiple s2ids via tcplPlot. Each query has an
#'   associated data table response for mocking an actual connection. Contains
#'   one 's2id' labeled item storing the ids used to load the data, for use in
#'   tests.}
#'   \item{plot_single_aeid}{List containing the queries used for loading and
#'   plotting sc data by aeid via tcplPlot. Each query has an associated data
#'   table response for mocking an actual connection. Contains one 'aeid'
#'   labeled item storing the id used to load the data, for use in tests.}
#'   \item{plot_multiple_aeid}{List containing the queries used for loading and
#'   plotting sc data by multiple aeids via tcplPlot. Each query has an
#'   associated data table response for mocking an actual connection. Contains
#'   one 'aeid' labeled item storing the ids used to load the data, for use in
#'   tests.}
#'   \item{plot_single_spid}{List containing the queries used for loading and
#'   plotting sc data by spid/aeid via tcplPlot. Each query has an associated
#'   data table response for mocking an actual connection. Contains 'spid' and
#'   'aeid' labeled items storing the ids used to load the data, for use in
#'   tests.}
#'   \item{plot_multiple_spid}{List containing the queries used for loading and
#'   plotting sc data by multiple spids/aeid via tcplPlot. Each query has an
#'   associated data table response for mocking an actual connection. Contains
#'   'spid' and 'aeid' labeled items storing the ids used to load the data, for
#'   use in tests.}
#'   \item{plot_single_s2id_compare}{List containing the queries used for 
#'   loading and plotting compared sc data by s2id via tcplPlot. Each query has
#'   an associated data table response for mocking an actual connection.
#'   Contains 's2id' and 'compare.s2id' labeled items storing the ids used to
#'   load the data, for use in tests.}
#'   \item{plot_multiple_s2id_compare}{List containing the queries used for loading and
#'   plotting compared sc data by multiple s2ids via tcplPlot. Each query has an
#'   associated data table response for mocking an actual connection. Contains 
#'   's2id' and 'compare.s2id' labeled items storing the ids used to load the 
#'   data, for use in tests.}
#'   \item{plot_single_aeid_compare}{List containing the queries used for
#'   loading and plotting compared sc data by aeid via tcplPlot. Each query has
#'   an associated data table response for mocking an actual connection.
#'   Contains 'aeid' and 'compare.aeid' labeled items storing the ids used to
#'   load the data, for use in tests.}
#'   \item{plot_multiple_aeid_compare}{List containing the queries used for
#'   loading and plotting compared sc data by multiple aeids via tcplPlot. Each
#'   query has an associated data table response for mocking an actual
#'   connection. Contains 'aeid' and 'compare.aeid' labeled items storing the
#'   ids used to load the data, for use in tests.}
#'   \item{plot_single_spid_compare}{List containing the queries used for
#'   loading and plotting compared sc data by spid/aeid via tcplPlot. Each query
#'   has an associated data table response for mocking an actual connection.
#'   Contains 'spid', 'compare.spid', 'aeid', and 'compare.aeid' labeled items
#'   storing the ids used to load the data, for use in tests.}
#'   \item{plot_multiple_spid_compare}{List containing the queries used for
#'   loading and plotting compared sc data by multiple spids/aeid via tcplPlot.
#'   Each query has an associated data table response for mocking an actual
#'   connection. Contains 'spid', 'compare.spid', 'aeid', and 'compare.aeid'
#'   labeled items storing the ids used to load the data, for use in tests.}
#' }
#' @source ToxCast database
"sc_test"


#' List containing ids used for different automated tests of tcpl integration
#' with the CTX APIs, randomly selected from what is available via API.
#' @format A list with 7 items:
#' \describe{
#'   \item{aeid}{Randomly selected assay component endpoint id}
#'   \item{acid}{Assay component id associated with the above aeid}
#'   \item{aid}{Assay id associated with the above aeid}
#'   \item{asid}{Assay source id associated with the above aeid}
#'   \item{dtxsid}{dsstox substance id of one sample from the above aeid}
#'   \item{spid}{Sample id of one (the same) sample from the above aeid}
#'   \item{m4id}{Level 4 id of one (the same) sample from the above aeid}
#' }
#' @source CTX Bioactivity API
"test_api"