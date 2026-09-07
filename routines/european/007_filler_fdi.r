library(MDstats); library(MD3); library(data.table)

# Set data directory
if (!exists("data_dir")) data_dir = getwd()
if (!exists("loaded_dir")) loaded_dir = data_dir

source('V:/FinFlows/githubrepo/finflows/routines/utilities.R')
gc()

## Load data
allfdi_pos_aggr <- readRDS(file.path(loaded_dir,'fdibuffer/allfdi_pos_aggr.rds'))
allfdi_pos_ctry <- readRDS(file.path(loaded_dir,'fdibuffer/allfdi_pos_ctry.rds'))
allfdi_flow_aggr <- readRDS(file.path(loaded_dir,'fdibuffer/allfdi_flow_aggr.rds'))
allfdi_flow_ctry <- readRDS(file.path(loaded_dir,'fdibuffer/allfdi_flow_ctry.rds'))

allfdi_pos_aggr <- do.call(rbind,allfdi_pos_aggr)
allfdi_pos_ctry <- do.call(rbind,allfdi_pos_ctry)
allfdi_flow_aggr <- do.call(rbind,allfdi_flow_aggr)
allfdi_flow_ctry <- do.call(rbind,allfdi_flow_ctry)

allfdi_pos_aggr <- as.md3(allfdi_pos_aggr,id.vars=c("REF_AREA","MEASURE","UNIT_MEASURE","MEASURE_PRINCIPLE",
                                            "ACCOUNTING_ENTRY","TYPE_ENTITY","FDI_COMP","SECTOR",           
                                            "COUNTERPART_AREA","LEVEL_COUNTERPART","ACTIVITY",
                                            "FDI_COLLECTION_ID","TIME"),obsattr = "_.obs_status")

allfdi_pos_ctry <- as.md3(allfdi_pos_ctry,id.vars=c("REF_AREA","MEASURE","UNIT_MEASURE","MEASURE_PRINCIPLE",
                                                    "ACCOUNTING_ENTRY","TYPE_ENTITY","FDI_COMP","SECTOR",           
                                                    "COUNTERPART_AREA","LEVEL_COUNTERPART","ACTIVITY",
                                                    "FDI_COLLECTION_ID","TIME"),obsattr = "_.obs_status")

allfdi_flow_aggr <- as.md3(allfdi_flow_aggr,id.vars=c("REF_AREA","MEASURE","UNIT_MEASURE","MEASURE_PRINCIPLE",
                                                    "ACCOUNTING_ENTRY","TYPE_ENTITY","FDI_COMP","SECTOR",           
                                                    "COUNTERPART_AREA","LEVEL_COUNTERPART","ACTIVITY",
                                                    "FDI_COLLECTION_ID","TIME"),obsattr = "_.obs_status")

allfdi_flow_ctry <- as.md3(allfdi_flow_ctry,id.vars=c("REF_AREA","MEASURE","UNIT_MEASURE","MEASURE_PRINCIPLE",
                                                    "ACCOUNTING_ENTRY","TYPE_ENTITY","FDI_COMP","SECTOR",           
                                                    "COUNTERPART_AREA","LEVEL_COUNTERPART","ACTIVITY",
                                                    "FDI_COLLECTION_ID","TIME"),obsattr = "_.obs_status")

# Dimensions (13 excl TIME):
#   REF_AREA . MEASURE . UNIT_MEASURE . MEASURE_PRINCIPLE . ACCOUNTING_ENTRY .
#   TYPE_ENTITY . FDI_COMP . SECTOR . COUNTERPART_AREA . LEVEL_COUNTERPART .
#   ACTIVITY . FREQ . FDI_COLLECTION_ID

## Exchange rate conversion

exr <- mds('ECB/EXR/A.USD.EUR.SP00.E')

allfdi_pos_aggr["............"] <- allfdi_pos_aggr["............"]/exr
allfdi_pos_ctry["............"] <- allfdi_pos_ctry["............"]/exr
allfdi_flow_aggr["............"] <- allfdi_flow_aggr["............"]/exr
allfdi_flow_ctry["............"] <- allfdi_flow_ctry["............"]/exr

## USD millions, so dont need to change order of magnitude

## Get Assets/Liabilitity = DO+DI
allfdi_pos_ctry["...A........."] <- allfdi_pos_ctry["...DO........."] + allfdi_pos_ctry["...DI........."]
allfdi_flow_ctry["...A........."] <- allfdi_flow_ctry["...DO........."] + allfdi_flow_ctry["...DI........."]

## DO, DI can be dropped later 

## Bind the 4 datasets
# First swap out FDI_COLLECTION_ID for STO dimension

allfdi_pos_aggr["...........LE."] <- allfdi_pos_aggr["...........AGGR."] 
allfdi_pos_ctry["...........LE."] <- allfdi_pos_ctry["...........CTRY_IND."] 
allfdi_flow_aggr["...........F."] <- allfdi_flow_aggr["...........AGGR."] 
allfdi_flow_ctry["...........F."] <- allfdi_flow_ctry["...........CTRY_IND."] 

# avoid duplicates - W also included in CTRY - AGGR not needed?
dimnames(allfdi_flow_aggr)$COUNTERPART_AREA[
  dimnames(allfdi_flow_aggr)$COUNTERPART_AREA == 'W'] <- 'WRL_REST'
dimnames(allfdi_pos_aggr)$COUNTERPART_AREA[
  dimnames(allfdi_pos_aggr)$COUNTERPART_AREA == 'W'] <- 'WRL_REST'



allfdi <- rbind(allfdi_pos_aggr,allfdi_pos_ctry,allfdi_flow_ctry,allfdi_flow_aggr)

# Recode instruments - this works better when I do it before converting data to md3, otherwise I have errors
map_instr <- c("LE_FA_F" = "F", "LE_FA_FL" = "F4", "LE_FA_F5" = "F5",
               "T_FA_F" = "F", "T_FA_FL" = "F4", "T_FA_F5A" = "F5A", "T_FA_F5B" = "F5B")

allfdi$MEASURE <- map_instr[allfdi$MEASURE]

allfdi <- subset(allfdi,FDI_COLLECTION_ID %in% c("LE","F"))
rm(allfdi_flow_aggr,allfdi_flow_ctry,allfdi_pos_aggr,allfdi_pos_ctry)

allfdi <- as.md3(allfdi,id.vars=c("REF_AREA","MEASURE","UNIT_MEASURE","MEASURE_PRINCIPLE",
                                                    "ACCOUNTING_ENTRY","TYPE_ENTITY","FDI_COMP","SECTOR",           
                                                    "COUNTERPART_AREA","LEVEL_COUNTERPART","ACTIVITY",
                                                    "FDI_COLLECTION_ID","TIME"),obsattr = "_.obs_status")


# dim(allfdi)
# allfdi[HUN.F.USD_EXC.A.L.ALL.D.S1.ARG.IMC._T.LE.2024] # Hungarian FDI liabilities vs. Argentina, should be 4.13 million EUR


## Data manipulation

# Rename dimensions

nms <- names(dimnames(allfdi))
nms[nms == "MEASURE"]           <- "INSTR"
nms[nms == "SECTOR"]              <- "REF_SECTOR"
nms[nms == "FDI_COMP"] <- "FUNCTIONAL_CAT"
nms[nms == "FDI_COLLECTION_ID"] <- "STO"
names(dimnames(allfdi)) <- nms

## Change country codes - issue with EU27 - EU27 and EU27_2020 are both assigned to EU27, which creates some duplicates?

exclude <- c("EU27", "EU27_2020")

# Recode only non-EU aggregate codes
non_eu_idx <- !dimnames(allfdi)$COUNTERPART_AREA %in% exclude

dimnames(allfdi)$REF_AREA <- ccode(
  dimnames(allfdi)$REF_AREA, 'iso3c', 'iso2m',
  leaveifNA = TRUE, warn = FALSE)

dimnames(allfdi)$COUNTERPART_AREA[non_eu_idx] <- ccode(
  dimnames(allfdi)$COUNTERPART_AREA[non_eu_idx], 'iso3c', 'iso2m',
  leaveifNA = TRUE, warn = FALSE)

dimnames(allfdi)$COUNTERPART_AREA[
  dimnames(allfdi)$COUNTERPART_AREA == 'CHN'] <- 'CN'


# Create F5 if mising

allfdi[".F5...........",onlyna=TRUE] <- allfdi[".F5A..........."] + allfdi[".F5B..........."]


## Export 
# Note ULT still retained for some reason, but can drop it
# Direction can be dropped as well
allfdi <- allfdi[...A......IMC...]
saveRDS(allfdi, file.path(loaded_dir,"aall_fdi.rds"))

### Now reduced number of dimensions. Should be REF_AREA, INSTR, ACCOUNTING_ENTRY (A/L), TYPE_ENTITY, COUNTERPART_AREA, STO, TIME
dim(allfdi)

#### FDI filling #####

## load filled iip bop
## split of assets and liabilities of banks
aa=readRDS(file.path(data_dir,'aa_iip_pip.rds')); gc()
ll=readRDS(file.path(data_dir,'ll_iip_pip.rds')); gc()


## Change frequency to Quarterly
frequency(allfdi) <- 'Q'

## Fill assets

# check: F4, _D is totally missing in particular, OECD data should fill this
aa["F4.BE+BG+CZ+DK+DE+EE+IE+GR+ES+FR+HR+IT+CY+LV+LT+LU+HU+MT+NL+AT+PL+PT+RO+SI+SK+FI+SE.S1.S1.LE._D.2024q4.BE+BG+CZ+DK+DE+EE+IE+GR+ES+FR+HR+IT+CY+LV+LT+LU+HU+MT+NL+AT+PL+PT+RO+SI+SK+FI+SE+US+CN+UK+BR"] 

aa["F..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F.A.ALL..."]
aa["F4..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F4.A.ALL..."]
aa["F5..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F5.A.ALL..."]
aa["F5A..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F5A.A.ALL..."] # Without reinvestment of earnings
aa["F5B..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F5B.A.ALL..."] # Reinvestment of earnings - these might be of interest later

aa["F4.BE+BG+CZ+DK+DE+EE+IE+GR+ES+FR+HR+IT+CY+LV+LT+LU+HU+MT+NL+AT+PL+PT+RO+SI+SK+FI+SE.S1.S1.LE._D.2024q4.BE+BG+CZ+DK+DE+EE+IE+GR+ES+FR+HR+IT+CY+LV+LT+LU+HU+MT+NL+AT+PL+PT+RO+SI+SK+FI+SE+US+CN+UK+BR"] 
## Should look much better

## Fill liabilities

ll["F4.WRL_REST.S1.S1.LE._D..DK"] # NAs before 2015q1

ll["F..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F.L.ALL..."]
ll["F4..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F4.L.ALL..."]
ll["F5..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F5.L.ALL..."]
ll["F5A..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F5A.L.ALL..."]
ll["F5B..S1.S1.._D..", usenames=TRUE, onlyna=TRUE] = allfdi[".F5B.L.ALL..."]

ll["F4.WRL_REST.S1.S1.LE._D..DK"] # Should be filled now (for Q4)

saveRDS(aa, file.path(data_dir,"aa_iip_fdi.rds"))
saveRDS(ll, file.path(data_dir,"ll_iip_fdi.rds"))

saveRDS(aa,file.path(data_dir,'vintages/aa_iip_fdi_' %&% format(Sys.time(),'%F') %&% '_.rds'))
saveRDS(ll,file.path(data_dir,'vintages/ll_iip_fdi_' %&% format(Sys.time(),'%F') %&% '_.rds'))
