library(broom)
library(caret)
library(cowplot)
library(data.table)
library(DescTools)
library(dplyr)
library(DT)
library(ggplot2)
library(ggpmisc)
library(gplots)
library(grid)
library(gridExtra)
library(gtable)
library(httk)
library(kableExtra)
library(openxlsx)
library(plotly)
library(purrr)

library(tidyr)
library(tcpl)
library(viridis)

library(RMySQL)
library(RPostgres)

RPostgres::Postgres()

con <- dbConnect(drv = RMySQL::MySQL(),
                 database=prod_toxrefdb_2_1, # internal research MySQL database naming currently
                 user="_dataminer", password="pass",host="ccte-mysql-res.epa.gov")
library(RPostgreSQL)
postgres.con <- dbConnect(drv = "PostgreSQL", user="_dataminer", password = "pass", db="res_toxref",
                          host = "ccte-postgres-res.dmap-prod.aws.epa.gov")
txrf <- dbGetQuery(postgres.con, "SELECT 
                   endpoint.endpoint_id,
                   endpoint.endpoint_category,
                   endpoint.endpoint_type,
                   endpoint.endpoint_target,
                   effect.effect_id,
                   effect.effect_desc,
                   effect.cancer_related,
                   tg_effect.tg_effect_id,
                   tg_effect.tg_id, 
                   tg_effect.life_stage,
                   tg_effect.effect_desc_free,
                   tg_effect.target_site,
                   tg_effect.direction, 
                   tg_effect.effect_comment,
                   tg_effect.no_quant_data_reported,
                   pod_tg_effect.pod_tg_effect_id,
                   pod_tg_effect.pod_id,
                   pod.pod_type,
                   pod.sex,
                   pod.admin_route,
                   pod.species,
                   pod.qualifier,
                   pod.pod_value, 
                   pod.pod_unit,
                   pod.mg_kg_day_value,
                   pod.dose_level,
                   pod.max_dose_level,
                   pod.staggered_dosing, 
                   pod.chemical_id,
                   pod.study_id,
                   pod.effect_profile_id,
                   pod.group_id,
                   chemical.dsstox_substance_id,
                   chemical.casrn,
                   chemical.preferred_name
                   FROM (((((prod_toxrefdb_2_1.endpoint 
                   JOIN prod_toxrefdb_2_1.effect ON endpoint.endpoint_id = effect.endpoint_id)
                   JOIN prod_toxrefdb_2_1.tg_effect ON effect.effect_id = tg_effect.effect_id)
                   JOIN prod_toxrefdb_2_1.pod_tg_effect 
                   ON tg_effect.tg_effect_id = pod_tg_effect.tg_effect_id)
                   JOIN prod_toxrefdb_2_1.pod ON pod_tg_effect.pod_id = pod.pod_id)
                   JOIN prod_toxrefdb_2_1.chemical ON pod.chemical_id = chemical.chemical_id)") %>%
  data.table()

save(txrf, file='txrf.RData')



# trying to determine which variables were duplicated, not necessary for vignette, but
# it is a good check
duplicated(colnames(txrf))
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [20] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE


#load(file='txrf.RData')
txrf_liver <- txrf %>% subset(endpoint_target =='liver'&life_stage == 'adult')
save(txrf_liver, file='txrf_liver.RData')

txrf_liver_chlor <- txrf %>% subset(dsstox_substance_id == 'DTXSID4020458' & endpoint_target =='liver' & life_stage == 'adult')
save(txrf_liver_chlor, file='txrf_liver_chlor.RData')


library(RMySQL)

mysql.con <- dbConnect(drv = RMySQL::MySQL(), user="_dataminer", pass="pass", db="prod_internal_invitrodb_v4_1",
                       host="ccte-mysql-res.epa.gov")


ace_tbl <- dbGetQuery(mysql.con, 'SELECT * FROM prod_internal_invitrodb_v4_1.assay INNER JOIN
                  prod_internal_invitrodb_v4_1.assay_component ON assay.aid = assay_component.aid INNER JOIN prod_internal_invitrodb_v4_1.assay_component_endpoint ON assay_component.acid=assay_component_endpoint.acid;') %>% data.table()


liver.aeid <- ace_tbl[tissue %in% 'liver' & cell_format %in% c('cell line','primary cell') & aid %in% c(2:6, 371, 399:401)]
datatable(liver.aeid[,c('aid','assay_name','tissue','organism','cell_format','cell_free_component_source','cell_short_name')])

#remove.packages("tcpl") 
#install.packages(tcpl)
library(tcpl)
tcplConf(user = "_dataminer", pass = "pass", db = "prod_internal_invitrodb_v4_1", drvr = "MySQL",
         host = "ccte-mysql-res.epa.gov")
tcplConfList()

mc5 <- tcplPrepOtpt(tcplSubsetChid(tcplLoadData(lvl=5, fld='aeid', val=liver.aeid[,aeid], type='mc')))
mc6 <- tcplPrepOtpt(tcplLoadData(lvl=6, fld='m4id', val=mc5$m4id, type='mc'))
setDT(mc6)
mc6_mthds <- mc6[ , .( mc6_mthd_id = paste(mc6_mthd_id, collapse=",")), by = m4id]
mc6_flags <- mc6[ , .( flag = paste(flag, collapse=";")), by = m4id]
mc5$mc6_flags <- mc6_mthds$mc6_mthd_id[match(mc5$m4id, mc6_mthds$m4id)]
mc5[, flag.length := ifelse(!is.na(mc6_flags), count.fields(textConnection(mc6_flags), sep =','), NA)]

# filter the dataset, with coarse filters
mc5[hitc==1 & flag.length < 3, use.me := 1]
mc5[hitc==1 & is.na(flag.length), use.me := 1]
mc5[hitc==1 & flag.length >= 3, use.me := 0]
mc5[fitc %in% c(36,45), use.me := 0]
mc5[hitc==-1, use.me := 0] # make hitc interpretable as a positive sum
mc5[use.me==0, modl_ga := as.numeric(NA)]
mc5[use.me==0, hitc := 0]
mc5[hitc==0, modl_ga := as.numeric(NA)]

mc5[hitc==1,ac50_uM := ifelse(!is.na(modl_ga), 10^modl_ga, NA)]

mc5.liver <- as.data.table(mc5)
mc5.liver[, organ := 'liver']

mc5_liver_chlor <- mc5.liver %>% subset(dsstox_substance_id.x == 'DTXSID4020458')

save(mc5_liver_chlor, file='mc5_liver_chlor.RData')



setnames(mc5_liver_chlor, c('dsstox_substance_id.x'), c('dsstox_substance_id'))
mc5_liver_chlor[, c('dsstox_substance_id.y') := NULL]

# ac50_uM is NA
mc5.dt.summary <-mc5_liver_chlor[,list(
  p5.ac50uM = quantile(ac50_uM, probs=c(0.05), na.rm=T),
  p50.ac50uM = quantile(ac50_uM, probs=c(0.50), na.rm=T),
  mean.ac50uM = mean(ac50_uM, na.rm=T))]

mc5.dt.summary[, names(mc5.dt.summary) := lapply(.SD, function(x) ifelse(is.nan(x), NA, x))]

mc5.dt.httk <- mc5.dt.summary[!is.na(mean.ac50uM)]