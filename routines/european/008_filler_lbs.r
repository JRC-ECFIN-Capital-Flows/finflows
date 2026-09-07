library(MDstats)
library(MD3)
library(data.table)

# Set data directory
if (!exists("data_dir")) data_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/'

source('V:/FinFlows/githubrepo/finflows/routines/utilities.R')
gc()


## Load by asset/liabilites split
aa=readRDS(file.path(data_dir,'aa_iip_fdi.rds')); gc()
ll=readRDS(file.path(data_dir,'ll_iip_fdi.rds')); gc()

gc()

## Load BIS LBS data (both Assets and Liabilites)
alllbs <- readRDS(file.path(data_dir,'lbsbuffer/alllbs.rds'))

alllbs <- do.call(rbind,alllbs)


alllbs <- as.md3(alllbs,
                 id.vars=c("L_MEASURE", "L_POSITION", "L_INSTR", "L_DENOM", "L_CURR_TYPE", "L_PARENT_CTY", "L_REP_BANK_TYPE", 
                                  "L_REP_CTY", "L_CP_SECTOR", "L_CP_COUNTRY", "L_POS_TYPE", "TIME"),
                 obsattr=c("_.obs_status","_.obs_conf","_.obs_pre_break"))

alllbs <- alllbs["..........."]

# Dimensions (11 excl TIME):
# L_POSITION (Claim/Liability), L_INSTR, L_REP_CTY, L_CP_SECTOR, L_CP_COUNTRY, L_POS_TYPE (All, Cross-border, Local)

## Exchange rate conversion

exr <- mds('ECB/EXR/Q.USD.EUR.SP00.E')

alllbs <- alllbs/exr

## USD millions, so dont need to change order of magnitude

# dim(alllbs)
# alllbs["C.A.AT.A.US.N.2024q4"] # Austrian MFI claims (assets) vs. US (S1), should be 10964 EUR


## Data manipulation

# Rename dimensions

nms <- names(dimnames(alllbs))
nms[nms == "L_POSITION"] <- "POS"
nms[nms == "L_INSTR"] <- "INSTR"
nms[nms == "L_REP_CTY"] <- "REF_AREA"
nms[nms == "L_CP_COUNTRY"] <- "COUNTERPART_AREA"
nms[nms == "L_CP_SECTOR"] <- "COUNTERPART_SECTOR"
names(dimnames(alllbs)) <- nms

# Recode instruments 
map_instr <- c("A" = "F", "B" = "F2_F3_F4","D" = "F3",
               "G" = "F2_F4", "M" = "F3S", "I" = "F7_F89")

dimnames(alllbs)$INSTR <- map_instr[dimnames(alllbs)$INSTR]

# Rename sectors 

## Check to see distribution - alllbs["C.F.US.A+B+F+M+P+I.5J..2022q4"]
# S12 = B + F =  A - P (non-bank)
# I ( Banks,related offices) and M (Banks, CB) seem like subsets of  B, but something missing
# So probably S12K = B = M + I + "Banks, unrelated offices", but latter not present
# Challenge: how to get S12T from these, perhaps cant 

# drop irrelevant sectors
alllbs <- alllbs["...A+B+C+F+G+H+M+N..."]

map_sector <- c("A" = "S1", "B" = "S12K", "C" = "S11",
                "F" = "S12_ext_S12K", "G" = "S13", "H" = "S1M",
                "M" = "S121", "N" = "S1_ext_S12K")

dimnames(alllbs)$COUNTERPART_SECTOR <- map_sector[dimnames(alllbs)$COUNTERPART_SECTOR]

## Change country codes - Already iso2 seemingly, only relevant aggregates should be renamed
#  Relevant: 5C (EA), 5J (W0), 

 dimnames(alllbs)$COUNTERPART_AREA[
   dimnames(alllbs)$COUNTERPART_AREA == '5C'] <- 'EA20'
 
 dimnames(alllbs)$COUNTERPART_AREA[
   dimnames(alllbs)$COUNTERPART_AREA == '5J'] <- 'W0'
 
 dimnames(alllbs)$REF_AREA[
   dimnames(alllbs)$REF_AREA == '5C'] <- 'EA20'

## Rename Claims to Assets to avoid confusion
 
dimnames(alllbs)$POS[
  dimnames(alllbs)$POS == 'C'] <- 'A'
 

## Compute S12 and S12T 
alllbs["...S12..."] <- NA
alllbs["...S12..."] <- alllbs["...S12K..."] + alllbs["...S12_ext_S12K..."]
alllbs["...S12T..."] <- NA
alllbs["...S12T..."] <- alllbs["...S12K..."] - alllbs["...S121..."]


## Calculate remaining instruments 
alllbs[".F3L....."] <- NA
alllbs[".F3L....."] <- alllbs[".F3....."] - alllbs[".F3S....."]

# alllbs[".F2_F4+F+F3+F3_F4.5A.S1.HU.N.2022q4"]
# Remember that F2_F4=F2+F4
# Assume that all loans are held against "non-banks"
alllbs["A.F4..S1..."] <- NA
alllbs["A.F4..S1..."] <- alllbs["A.F2_F4..S1_ext_S12K..."]
alllbs["A.F2..S1..."] <- NA
alllbs["A.F2..S1..."] <- alllbs["A.F2_F4..S1..."] - alllbs["A.F2_F4..S1_ext_S12K..."]
# Banks only hold loans vs Households
alllbs["A.F4..S1M..."] <- NA
alllbs["A.F4..S1M..."] <- alllbs["A.F2_F4..S1M..."]
# Banks only hold loans vs NFCs
alllbs["A.F4..S11..."] <- NA
alllbs["A.F4..S11..."] <- alllbs["A.F2_F4..S11..."]
# Banks only hold loans vs GOV
alllbs["A.F4..S13..."] <- NA
alllbs["A.F4..S13..."] <- alllbs["A.F2_F4..S13..."]
# Banks only hold deposits vs Banks
alllbs["A.F2..S12K..."] <- NA
alllbs["A.F2..S12K..."] <- alllbs["A.F2_F4..S12K..."]
alllbs["A.F2..S121..."] <- NA
alllbs["A.F2..S121..."] <- alllbs["A.F2_F4..S121..."]
alllbs["A.F2..S12T..."] <- NA
alllbs["A.F2..S12T..."] <- alllbs["A.F2_F4..S12T..."]

# Assume that banks don't hold any loans on the liability side (only deposits)
alllbs["L.F2....."] <- NA
alllbs["L.F2....."] <- alllbs["L.F2_F4....."]

### Start filling (only _T and _O=_T for cross-border F2, F4 only)
# recall 6th dimension: A (all), R (local), N (cross-border)

aa["F+F2+F4.AT.S12K.S1.LE._T+_O.2022q4.BE"] # should be NA


# Fill totals first
aa["..S12K..LE._T..W0", usenames=TRUE, onlyna=TRUE] <- alllbs["A....W0.A.1998q4:"]
aa["..S12K..LE._T..W2", usenames=TRUE, onlyna=TRUE] <- alllbs["A....W0.R.1998q4:"]

# Create list of cross-border counterpart areas
all_ca <- dimnames(aa)$COUNTERPART_AREA
ca_vec <- all_ca[!all_ca %in% c("W0", "W2","WRL_REST")]
ca_string <- paste(ca_vec, collapse = "+")

# LBS doesn't distinguish between functional categories
aa[paste("..S12K..LE._T..",ca_string), usenames=TRUE, onlyna=TRUE] <- alllbs["A.....N.1998q4:"]
# But we can assume _T=_O for F2 and F4
aa[paste("F2..S12K..LE._O..",ca_string), usenames=TRUE, onlyna=TRUE] <- alllbs["A.....N.1998q4:"]
aa[paste("F4..S12K..LE._O..",ca_string), usenames=TRUE, onlyna=TRUE] <- alllbs["A.....N.1998q4:"]
# Run separately for WRL_REST, as it is not the dimnames of LBS
aa["..S12K..LE._T..WRL_REST", usenames=TRUE, onlyna=TRUE] <- alllbs["A....W0.N.1998q4:"]
aa["F2..S12K..LE._O..WRL_REST", usenames=TRUE, onlyna=TRUE] <- alllbs["A....W0.N.1998q4:"]
aa["F4..S12K..LE._O..WRL_REST", usenames=TRUE, onlyna=TRUE] <- alllbs["A....W0.N.1998q4:"]


aa["F+F2+F4.AT.S12K.S1.LE._T+_O.2022q4.BE"] # should be filled now

# do the same for liabilities

ll["F+F2.BE.S1.S12K.LE._T+_O.2022q4.AT"]

ll[".W0..S12K.LE._T..", usenames=TRUE, onlyna=TRUE] <- alllbs["L.....A.1998q4:"]
ll[".W2..S12K.LE._T..", usenames=TRUE, onlyna=TRUE] <- alllbs["L.....R.1998q4:"]
ll[paste(ca_string,"F2...S12K.LE._O.."), usenames=TRUE, onlyna=TRUE] <- alllbs["L.....N.1998q4:"]
ll[paste(ca_string,"F4...S12K.LE._O.."), usenames=TRUE, onlyna=TRUE] <- alllbs["L.....N.1998q4:"]
ll[paste(ca_string,"...S12K.LE._T.."), usenames=TRUE, onlyna=TRUE] <- alllbs["L.....N.1998q4:"]

ll["F+F2.BE.S1.S12K.LE._T+_O.2022q4.AT"] # should be filled now

## Export 
gc()
saveRDS(aa,file.path(data_dir,'aa_iip_lbs.rds'))
saveRDS(ll,file.path(data_dir,'ll_iip_lbs.rds'))

saveRDS(aa,file.path(data_dir,'vintages/aa_iip_lbs_' %&% format(Sys.time(),'%F') %&% '_.rds'))
saveRDS(ll,file.path(data_dir,'vintages/ll_iip_lbs_' %&% format(Sys.time(),'%F') %&% '_.rds'))

