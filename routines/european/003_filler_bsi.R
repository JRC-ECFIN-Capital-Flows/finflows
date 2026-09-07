

library(MDstats); library(MD3)

# Set data directory
if (!exists("data_dir")) data_dir = getwd()

## load filled iip bop
## split of assets and liabilities of banks
aa=readRDS(file.path(data_dir,'aa_iip_cps.rds')); gc()
ll=readRDS(file.path(data_dir,'ll_iip_cps.rds')); gc()

# Load loans data

loans_bsi= readRDS(file.path(data_dir, "bsi_loans.rds")); gc()

###  manual allocation of U5 and U2 to EA20 [U2 would be including the reference area]
dimnames(loans_bsi)$REF_AREA[dimnames(loans_bsi)$REF_AREA=='U5'] ='EA20'
dimnames(loans_bsi)$REF_AREA[dimnames(loans_bsi)$REF_AREA=='U2'] ='EA20'
dimnames(loans_bsi)$COUNTERPART_AREA[dimnames(loans_bsi)$COUNTERPART_AREA=='U5'] ='EA20'
dimnames(loans_bsi)$COUNTERPART_AREA[dimnames(loans_bsi)$COUNTERPART_AREA=='U2'] ='EA20'

dimnames(loans_bsi)[['REF_AREA']] = ccode(dimnames(loans_bsi)[['REF_AREA']],2,'iso2m',leaveifNA=TRUE); gc()
dimnames(loans_bsi)[['COUNTERPART_AREA']] = ccode(dimnames(loans_bsi)[['COUNTERPART_AREA']],2,'iso2m',leaveifNA=TRUE); gc()

#LOANS FILLING
# the MFI is the creditor, so REF_AREA and REF_SECTOR = S12T are the aa convention already
# maturity A is = total, F= up to 1 year (short term), K = more than 1 year (long term)
aa[F4..S12T..._T.., usenames=TRUE, onlyna=TRUE] = loans_bsi[".A....1998q4:"]
aa[F4..S12T..._O.., usenames=TRUE, onlyna=TRUE] = loans_bsi[".A....1998q4:"]
aa[F4S..S12T..._T.., usenames=TRUE, onlyna=TRUE] = loans_bsi[".F....1998q4:"]
aa[F4S..S12T..._O.., usenames=TRUE, onlyna=TRUE] = loans_bsi[".F....1998q4:"]
aa[F4L..S12T..._T.., usenames=TRUE, onlyna=TRUE] = loans_bsi[".K....1998q4:"]
aa[F4L..S12T..._O.., usenames=TRUE, onlyna=TRUE] = loans_bsi[".K....1998q4:"]

# Load deposits data
deposits_bsi = readRDS(file.path(data_dir, "bsi_deposits.rds")); gc()

###  manual allocation of U5 and U2 to EA20
dimnames(deposits_bsi)$REF_AREA[dimnames(deposits_bsi)$REF_AREA=='U5'] ='EA20'
dimnames(deposits_bsi)$REF_AREA[dimnames(deposits_bsi)$REF_AREA=='U2'] ='EA20'
dimnames(deposits_bsi)$COUNTERPART_AREA[dimnames(deposits_bsi)$COUNTERPART_AREA=='U5'] ='EA20'
dimnames(deposits_bsi)$COUNTERPART_AREA[dimnames(deposits_bsi)$COUNTERPART_AREA=='U2'] ='EA20'

dimnames(deposits_bsi)[['REF_AREA']] = ccode(dimnames(deposits_bsi)[['REF_AREA']],2,'iso2m',leaveifNA=TRUE); gc()
dimnames(deposits_bsi)[['COUNTERPART_AREA']] = ccode(dimnames(deposits_bsi)[['COUNTERPART_AREA']],2,'iso2m',leaveifNA=TRUE); gc()

# 006 names the depositor sector REF_SECTOR. In ll the depositor is the creditor,
# so it becomes COUNTERPART_SECTOR. The two geography dimensions are already in
# the right roles and are not touched.
names(dimnames(deposits_bsi))[names(dimnames(deposits_bsi))=="REF_SECTOR"] <- 'COUNTERPART_SECTOR'

#DEPOSITS FILLING
ll[F2M...S12T.._T.., usenames=TRUE, onlyna=TRUE] = deposits_bsi[".A....1998q4:"]
ll[F2M...S12T.._O.., usenames=TRUE, onlyna=TRUE] = deposits_bsi[".A....1998q4:"]

gc()
saveRDS(aa,file.path(data_dir,'aa_iip_bsi.rds'))
saveRDS(ll,file.path(data_dir,'ll_iip_bsi.rds'))

saveRDS(aa,file.path(data_dir,'vintages/aa_iip_bsi_' %&% format(Sys.time(),'%F') %&% '_.rds'))
saveRDS(ll,file.path(data_dir,'vintages/ll_iip_bsi_' %&% format(Sys.time(),'%F') %&% '_.rds'))
