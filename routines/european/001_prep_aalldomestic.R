library(MDstats) 
library(MD3)

# Set data directory
#data_dir= file.path(getwd(),'data')
if (!exists("data_dir")) data_dir = getwd()


############################################################################################################################
############################################################################################################################
#####                                   temp load current aall version                                                 ##### 
##        aall_domestic is the final output of the domestic economy (incl. for example F51M counterpart allocation)       ##
############################################################################################################################
############################################################################################################################

aall=readRDS(file.path(data_dir,'intermediate_domestic_data_files/aall_domestic.rds')); gc()
gc()
gc()
dimnames(aall)
aall[F.AT.S1.S2+S0.LE._T.2022q4]
aall[F.AT.S2+S0.S1.LE._T.2022q4]


#########################################
#check functional_cat - only use _T 
#########################################

if (!('FUNCTIONAL_CAT' %in% names(dimnames(aall)))) stop('hey, i need FUNCTIONAL_CAT')
dimnames(aall)[[6]]

aall=aall['....._T.'] 
aallsmall=aall
aall=add.dim(aallsmall, .dimname = 'FUNCTIONAL_CAT', .dimcodes = c('_T'), .fillall = FALSE)
aall <- aperm(aall, c(2:6,1,7))
gc()
dimnames(aall)[[6]]
dimnames(aall)$FUNCTIONAL_CAT[dimnames(aall)$FUNCTIONAL_CAT=='X_T'] = '_T'
dimnames(aall)
aall[.AT.S0+S1+S2.S11.LE._T.2022q4]

###to avoid "non unique values" error, we only keep one EA and EU element
aall[.U2.....,onlyna=TRUE]<-aall[.EA.....]
aall[.EU27.....,onlyna=TRUE]<-aall[.EU27_2020.....]

aall=aall[.AT+BE+BG+CY+CZ+DE+DK+U2+EA18+EA19+EA20+EE+ES+FI+FR+HR+HU+IE+IT+LT+LU+LV+MT+NL+PL+PT+RO+SE+SI+SK+EL+UK+CH+NO+IS+TR+AL+RS+MK+U5+XK+MD+BA+ME+EU27+U4+U6+Z5+U3+U9+U8.....]

#########################################
#clean data 
#########################################

dall=as.data.table(aall,.simple=TRUE,na.rm=TRUE)
if (anyNA(dall$obs_value)) {dall=dall[!is.na(obs_value), ]}
dall=dall[!(REF_SECTOR %in% c('S2I','S22', 'S21')),]
dall=dall[!(COUNTERPART_SECTOR %in% c('S2I','S22', 'S21')),]
dall=dall[!(REF_AREA %in% c('GR','GB','I7','I8','I9')),]

#dall=dall[!(COUNTERPART_AREA %in% c('GR','GB','I7','I8','I9')),]

mydn=dimnames(aall)
mydn[["REF_SECTOR"]]=setdiff(mydn[["REF_SECTOR"]],c('S2I','S22', 'S21'))
mydn[["COUNTERPART_SECTOR"]]=setdiff(mydn[["COUNTERPART_SECTOR"]],c('S2I','S22', 'S21'))
mydn[["REF_AREA"]]=setdiff(mydn[["REF_AREA"]],c('GR','GB','I7','I8','I9'))
#mydn[["COUNTERPART_AREA"]]=setdiff(mydn[["COUNTERPART_AREA"]],c('GR','GB','I7','I8','I9'))
attr(dall, 'dcstruct') = MD3:::.dimcodesrescue(mydn,dimcodes(aall))
attr(dall, 'dcsimp') = mydn
aall=as.md3(dall)
gc()

# Add back FUNCTIONAL_CAT dimension (it was lost during data.table conversion)
aall=add.dim(aall, .dimname = 'FUNCTIONAL_CAT', .dimcodes = c('_T'), .fillall = FALSE)

aall <- aperm(aall, c(2, 3, 4, 5, 6, 1, 7))
dimnames(aall)$FUNCTIONAL_CAT[dimnames(aall)$FUNCTIONAL_CAT=='X_T'] = '_T'

#########################################
#adjust iso code
#########################################

#ccode(dimnames(loans_bsi)[['REF_AREA']],2,'iso2m',leaveifNA=TRUE)

dimnames(aall)[['REF_AREA']] = ccode(dimnames(aall)[['REF_AREA']],2,'iso2m'); gc()

aall[F.AT.S1.S2+S0.LE._T.2022q4]
aall[F.AT.S2+S0.S1.LE._T.2022q4]


saveRDS(aall,file=file.path(data_dir,'aall_temp.rds'))
saveRDS(aall,file=file.path(data_dir,'vintages/aall_temp' %&% format(Sys.time(),'%F') %&% '_.rds'))

##############################################################
### load FDI Information from ECB QSA
##############################################################
source(file.path(script_dir, "european/fndloader0.R"))

fnd=readRDS(file.path(data_dir, 'fndqsa.rds'))
names(dimnames(fnd))[5] = 'INSTR'
fnd=aperm(copy(fnd),c(5,2,3,1,4,6))


##############################################################
### fill FDI Information from ECB QSA
##############################################################
#setting FDI information from QSA as NA if 0, in order to fill if values are available in ESTAT
##############################################################
dall=as.data.table(unflag(aall),na.rm = TRUE)
dall[COUNTERPART_SECTOR =='S0' & FUNCTIONAL_CAT   =='_D' & obs_value ==0, obs_value:=NA]
dall[REF_SECTOR =='S0' & FUNCTIONAL_CAT   =='_D' & obs_value ==0, obs_value:=NA]
aall=as.md3(dall); rm(dall)
###direct investment assets from ECB QSA
aall[...S2.LE._D., usenames=TRUE, onlyna=TRUE] = fnd[...S0.LE.]
aall[..S12R.S2.LE._D., usenames=TRUE, onlyna=TRUE] = fnd[..S124.S0.LE.]+fnd[..S12O.S0.LE.]+fnd[..S12Q.S0.LE.]
aall[...S2.F._D., usenames=TRUE, onlyna=TRUE] = fnd[...S0.F.]
aall[..S12R.S2.F._D., usenames=TRUE, onlyna=TRUE] = fnd[..S124.S0.F.]+fnd[..S12O.S0.F.]+fnd[..S12Q.S0.F.]
###direct investment liabilities from ECB QSA
aall[..S2..LE._D., usenames=TRUE, onlyna=TRUE] = fnd[..S0..LE.]
aall[..S2.S12R.LE._D., usenames=TRUE, onlyna=TRUE] = fnd[..S0.S124.LE.]+fnd[..S0.S12O.LE.]+fnd[..S0.S12Q.LE.]
aall[..S2..F._D., usenames=TRUE, onlyna=TRUE] = fnd[..S0..F.]
aall[..S2.S12R.F._D., usenames=TRUE, onlyna=TRUE] = fnd[..S0.S124.F.]+fnd[..S0.S12O.F.]+fnd[..S0.S12Q.F.]

gc()
str(aall)

saveRDS(aall,file=file.path(data_dir,'aall_intermediate.rds'))
saveRDS(aall,file=file.path(data_dir,'vintages/aall_intermediate' %&% format(Sys.time(),'%F') %&% '_.rds'))

###############################################################
# Add the new dimension COUNTERPART_AREA with W0, W2, WRL_REST
###############################################################

aallsmall = copy(aall)
gc()
aall=add.dim(aallsmall, .dimname = 'COUNTERPART_AREA', .dimcodes = c('W2', 'W0', 'WRL_REST'), .fillall = FALSE)
aall <- aperm(aall, c(2:length(dim(aall)),1))
gc()

#check: all data sits at W2 at this point
aall[F.AT.S1..LE._T.2022q4.]
aall[F.AT..S1.LE._T.2022q4.]

## Blank out non-_T functional categories in the new 8-dim object
aall['....._F+_O+_P+_R..'] <- NA

###############################################################
# Save aall_intermediate (8-dim, before asset/liability split)
###############################################################

saveRDS(aall,file=file.path(data_dir,'aall_intermediate.rds'))
saveRDS(aall,file=file.path(data_dir,'vintages/aall_intermediate' %&% format(Sys.time(),'%F') %&% '_.rds'))


##############################################################
#####splitting aall into assets (aa) and liabilities (ll)#####
###to fill cross-border assets and liabilities separately ####
##############################################################
##
## aa and ll contain the same data. The same query on both objects
## returns the same economic fact from two perspectives:
##   aa[F.AT.S11.S1.LE._T.2022q4.WRL_REST] = AT's S11 cross-border assets
##   ll[F.AT.S11.S1.LE._T.2022q4.WRL_REST] = foreign S1 liabilities to AT's S11
##
## In aa:  INSTR, REF_AREA, REF_SECTOR, COUNTERPART_SECTOR, STO, FUNCTIONAL_CAT, TIME, COUNTERPART_AREA
## In ll:  INSTR, COUNTERPART_AREA, COUNTERPART_SECTOR, REF_SECTOR, STO, FUNCTIONAL_CAT, TIME, REF_AREA
##
## The positions carry the same economic meaning:
##   pos 2: where the entity is (AT)
##   pos 3: the entity (S11)
##   pos 4: the counterpart (S1)
##   pos 8: where the counterpart is (WRL_REST)
##############################################################


##############################################################
####                      ASSETS (aa)                     ####
##############################################################
gc()

## Step 1: Map COUNTERPART_SECTOR S0/S2 from W2 to W0/WRL_REST
aa = copy(aall)
gc()


## Step 2: Relabel COUNTERPART_SECTOR S2→S1 at WRL_REST, S0→S1 at W0
aa[...S1....WRL_REST, usenames=TRUE, onlyna=TRUE] <- aa[...S2....W2]; gc()
aa[...S1....W0,       usenames=TRUE, onlyna=TRUE] <- aa[...S0....W2]; gc()
aa[...S1....W2,       usenames=TRUE, onlyna=TRUE] <- aa[...S1....W2]; gc()

aa[..S1.....WRL_REST, usenames=TRUE, onlyna=TRUE] <- aa[..S2.....W2]; gc()
aa[..S1.....W0,       usenames=TRUE, onlyna=TRUE] <- aa[..S0.....W2]; gc()
aa[..S1.....W2,       usenames=TRUE, onlyna=TRUE] <- aa[..S1.....W2]; gc()


## Step 3: Exclude S0/S2 from REF_SECTOR (not needed for asset perspective)
tempix = dimnames(aa)[[3]]
aa = aa[,, setdiff(tempix, c('S0', 'S2')),,,,,]
gc()


aa[F.AT.S1.S11+S12K+S1+S1M.LE._T.2022q4.W0+W2+WRL_REST]
aa[F.AT.S11+S12K+S1+S1M.S1.LE._T.2022q4.W0+W2+WRL_REST]

dimnames(aa)

# aa=aa[..S121+S12T+S124+S12O+S12Q+S13+S11+S1M+S0+S1+S12K+S12R+S12+S125+S126+S127+S128+S129+S14+S15+S122+S123+S12P+S125A+S1311+S1P+S12M+S1V+S1Z+S11_S14_S15+S12V+S1X.S1M+S11+S12O+S12Q+S124+S13+S1+S12K+S0+S12R+S121+S12T+S12+S125+S126+S127+S128+S129+S14+S15+S122+S123+S125A+S12P+S1311+S1P+S12M+S1V+S11_S14_S15+S1X+S1Z....]

##############################################################
####                   LIABILITIES (ll)                   ####
#### Same data as aa, dimension names swapped:            ####
####   REF <-> COUNTERPART for both AREA and SECTOR       ####
##############################################################

ll = copy(aa)
ll_W <- aall[..S0+S2+S1.....]

dim(ll)
dim(ll_W)
###verbindlichkeiten S11 ll_W[F.AT.S0+S2+S1.S11.LE._T.2022q4.W2] 
# dim(ll_W)
# INSTR           REF_AREA         REF_SECTOR COUNTERPART_SECTOR              STO     FUNCTIONAL_CAT 
# 38                 51                  3                 32                  2                  6 
# TIME   COUNTERPART_AREA 
# 117                  3

names(dimnames(ll_W))[4] = 'TEMP_SECTOR'
names(dimnames(ll_W))[3] = 'COUNTERPART_SECTOR'
names(dimnames(ll_W))[4] = 'REF_SECTOR'

ll[...S1....WRL_REST, usenames=TRUE, onlyna=FALSE] <- ll_W[..S2.....W2]; gc()
ll[...S1....W0,       usenames=TRUE, onlyna=FALSE] <- ll_W[..S0.....W2]; gc()
ll[...S1....W2,       usenames=TRUE, onlyna=FALSE] <- ll_W[..S1.....W2]; gc()

tempix = dimnames(ll)[[3]]
ll = ll[,, setdiff(tempix, c('S0', 'S2')),,,,,]
gc()


#switching structure of CP AREA and CP Sector for liabilities
ll=aperm(copy(ll), c(1,8,4,3,5,6,7,2))
dim(ll)

ll[F..S1.S11.LE._T.2022q4.AT]


saveRDS(aa, file.path(data_dir, 'aa_prep.rds'))
saveRDS(ll, file.path(data_dir, 'll_prep.rds'))

saveRDS(aa, file.path(data_dir, 'vintages/aa_prep' %&% format(Sys.time(),'%F') %&% '_.rds'))
saveRDS(ll, file.path(data_dir, 'vintages/ll_prep' %&% format(Sys.time(),'%F') %&% '_.rds'))


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
#####origal below
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################


##############################################################
####                      ASSETS (aa)                     ####
##############################################################
# gc()
# 
# ## Step 1: Map COUNTERPART_SECTOR S0/S2 from W2 to W0/WRL_REST
# aa = copy(aall)
# aa_W <- aa[..S0+S2.....]
# 
# 
# aa[...S1....W0,       usenames=TRUE, onlyna=TRUE] <- aa[...S0....W2]; gc()
# aa[...S1....WRL_REST, usenames=TRUE, onlyna=TRUE] <- aa[...S2....W2]; gc()
# aa[...S1....W2, usenames=TRUE, onlyna=TRUE] <- aa[...S1....W2]; gc()
# 
# 
# 
# names(dimnames(aa_W))[2] = 'TEMP_AREA'
# names(dimnames(aa_W))[8] = 'REF_AREA'
# names(dimnames(aa_W))[2] = 'COUNTERPART_AREA'
# 
# aa_W[..S1.....W0]<-aa_W[..S0.....W2]; gc()
# aa_W[..S1.....WRL_REST]<-aa_W[..S2.....W2]; gc()
# 
# 
# aa[..S1....., usenames=TRUE, onlyna=TRUE]<-aa_W[..S1.....]
# 
# 
# 
# gc()
# 
# aa[.WRL_REST+W0+W2.S1.S11.LE._T.2022q4.AT]
# aa[.AT.S11.S1.LE._T.2022q4.WRL_REST+W0+W2]
# aa[.AT.S1.S11.LE._T.2022q4.WRL_REST+W0+W2]
# 
# 
# 
# 
# # ## Step 2: Exclude S0/S2 from REF_SECTOR (not needed for asset perspective)
# # ###tempix = dimnames(aa)[[3]]
# # ###aa = aa[,, setdiff(tempix, c('S0', 'S2')),,,,,]
# # gc()
# # 
# # ## Step 3: Relabel COUNTERPART_SECTOR S2→S1 at WRL_REST, S0→S1 at W0
# # aa[...S1....WRL_REST, usenames=FALSE, onlyna=TRUE] <- aa[...S2....WRL_REST]; gc()
# # aa[...S1....W0,       usenames=FALSE, onlyna=TRUE] <- aa[...S0....W0];       gc()
# # 
# # aa[..S1.....WRL_REST, usenames=FALSE, onlyna=TRUE] <- aa[..S2.....WRL_REST]; gc()
# # aa[..S1.....W0,       usenames=FALSE, onlyna=TRUE] <- aa[..S0.....W0];       gc()
# # 
# # 
# # aa[F.AT.S1.S11+S12K+S1+S1M.LE._T.2022q4.W0+W2+WRL_REST]
# # aa[F.AT.S11+S12K+S1+S1M.S1.LE._T.2022q4.W0+W2+WRL_REST]
# # 
# # aa[...S2....WRL_REST] = aa[..S2.....WRL_REST]  <-NA
# # 
# # aa[F.AT.S1.S11+S12K+S1+S1M.LE._T.2022q4.W0+W2+WRL_REST]
# # aa[F.AT.S11+S12K+S1+S1M.S1.LE._T.2022q4.W0+W2+WRL_REST]
# # 
# # dimnames(aa)
# # 
# # aa=aa[..S121+S12T+S124+S12O+S12Q+S13+S11+S1M+S0+S1+S12K+S12R+S12+S125+S126+S127+S128+S129+S14+S15+S122+S123+S12P+S125A+S1311+S1P+S12M+S1V+S1Z+S11_S14_S15+S12V+S1X.S1M+S11+S12O+S12Q+S124+S13+S1+S12K+S0+S12R+S121+S12T+S12+S125+S126+S127+S128+S129+S14+S15+S122+S123+S125A+S12P+S1311+S1P+S12M+S1V+S11_S14_S15+S1X+S1Z....]
# 
# ##############################################################
# ####                   LIABILITIES (ll)                   ####
# #### Same data as aa, dimension names swapped:            ####
# ####   REF <-> COUNTERPART for both AREA and SECTOR       ####
# ##############################################################
# 
# ll = copy(aa)
# 
# names(dimnames(ll))[2] = 'TEMP_AREA'
# names(dimnames(ll))[8] = 'REF_AREA'
# names(dimnames(ll))[2] = 'COUNTERPART_AREA'
# 
# names(dimnames(ll))[4] = 'TEMP_SECTOR'
# names(dimnames(ll))[3] = 'COUNTERPART_SECTOR'
# names(dimnames(ll))[4] = 'REF_SECTOR'
# 
# names(dimnames(ll))
# 
# # Test: same query, same values
# aa[F.AT.S11.S1.LE._T.2022q4.W0]
# ll[F.AT.S11.S1.LE._T.2022q4.W0]
# aa[F.AT.S11.S1.LE._T.2022q4.WRL_REST]
# ll[F.AT.S11.S1.LE._T.2022q4.WRL_REST]
# 
# ll[F.AT.S1.S11+S12K+S1+S1M.LE._T.2022q4.W0+W2+WRL_REST]
# ll[F.AT.S11+S12K+S1+S1M.S1.LE._T.2022q4.W0+W2+WRL_REST]
# 
# 
# gc()
# 
# 
# 
# saveRDS(aa, file.path(data_dir, 'aa_prep.rds'))
# saveRDS(ll, file.path(data_dir, 'll_prep.rds'))
# 
# saveRDS(aa, file.path(data_dir, 'vintages/aa_prep' %&% format(Sys.time(),'%F') %&% '_.rds'))
# saveRDS(ll, file.path(data_dir, 'vintages/ll_prep' %&% format(Sys.time(),'%F') %&% '_.rds'))
