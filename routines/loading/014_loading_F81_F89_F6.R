# ==============================================================================
# 012_loading_F81_F89_F6.R
# Consolidated loading of F81 (trade credits), F89 (other accounts), 
# and F6 (insurance & pensions) data from ECB/QSA and Eurostat BoP
# ==============================================================================

# Load required packages
library(MDstats)
library(MD3)

# Set data directory
if (!exists("data_dir")) data_dir = getwd()

# ==============================================================================
# F81 - TRADE CREDITS
# ==============================================================================

# --- ECB QSA ---
tradecredits_al=mds('ECB/QSA/Q.N..W0+W1...N.A+L.LE.F81.T._Z.XDC._T.S.V.N._T')
saveRDS(tradecredits_al, file.path(data_dir, 'tradecredits_al.rds'))

# --- Eurostat BoP ---
bopf81q=mds('Estat/bop_iip6_q/Q.MIO_EUR.FA__D__F81+FA__O__F81+..S1.A_LE+L_LE.WRL_REST.')
bopf81_a=mds('Estat/bop_iip6_q/A.MIO_EUR.FA__D__F81+FA__O__F81+..S1.A_LE+L_LE.WRL_REST.')
saveRDS(bopf81q, file.path(data_dir, 'bopf81q.rds'))

bopf81a=copy(bopf81_a); frequency(bopf81a)='Q'
saveRDS(bopf81a, file.path(data_dir, 'bopf81a.rds'))

# --- Counterpart sector processing ---
names(dimnames(tradecredits_al)) =gsub("^COUNTER.*$","COUNTERPART_SECTOR",names(dimnames(tradecredits_al)))
tradecredits_al=aperm(tradecredits_al,c("REF_AREA","REF_SECTOR","COUNTERPART_SECTOR","ACCOUNTING_ENTRY","TIME"))

tradecredits=tradecredits_al[...A.]

dimnames(tradecredits)$COUNTERPART_SECTOR = c(W0='S0' , W1='S2')[dimnames(tradecredits)$COUNTERPART_SECTOR]
tradecredits[.S2.S0.] =tradecredits_al[.S1.W1.L.]
tradecredits[.S2.S1.] =tradecredits_al[.S1.W1.L.]
tradecredits[.S1.S1.] = tradecredits[.S1.S0.] -tradecredits[.S1.S2.] 
tradecredits[.S0.S1.] = tradecredits[.S1.S1.] +tradecredits[.S2.S1.] 
saveRDS(tradecredits, file.path(data_dir, 'tradecredits.rds'))
gc()

# ==============================================================================
# F89 - OTHER ACCOUNTS
# ==============================================================================

# --- ECB QSA ---
otheraccounts_al=mds('ECB/QSA/Q.N..W0+W1...N.A+L.LE.F89.T._Z.XDC._T.S.V.N._T')
saveRDS(otheraccounts_al, file.path(data_dir, 'otheraccounts_al.rds'))

# --- Eurostat BoP ---
bopf89q=mds('Estat/bop_iip6_q/Q.MIO_EUR.FA__O__F89..S1.A_LE+L_LE.WRL_REST.')
bopf89_a=mds('Estat/bop_iip6_q/A.MIO_EUR.FA__O__F89..S1.A_LE+L_LE.WRL_REST.')
saveRDS(bopf89q, file.path(data_dir, 'bopf89q.rds'))

bopf89a=copy(bopf89_a); frequency(bopf89a)='Q'
saveRDS(bopf89a, file.path(data_dir, 'bopf89a.rds'))

# --- Counterpart sector processing ---
names(dimnames(otheraccounts_al)) =gsub("^COUNTER.*$","COUNTERPART_SECTOR",names(dimnames(otheraccounts_al)))
otheraccounts_al=aperm(otheraccounts_al,c("REF_AREA","REF_SECTOR","COUNTERPART_SECTOR","ACCOUNTING_ENTRY","TIME"))

otheraccounts=otheraccounts_al[...A.]

dimnames(otheraccounts)$COUNTERPART_SECTOR = c(W0='S0' , W1='S2')[dimnames(otheraccounts)$COUNTERPART_SECTOR]
otheraccounts[.S2.S0.] =otheraccounts_al[.S1.W1.L.]
otheraccounts[.S2.S1.] =otheraccounts_al[.S1.W1.L.]
otheraccounts[.S1.S1.] = otheraccounts[.S1.S0.] -otheraccounts[.S1.S2.] 
otheraccounts[.S0.S1.] = otheraccounts[.S1.S1.] +otheraccounts[.S2.S1.] 
saveRDS(otheraccounts, file.path(data_dir, 'otheraccounts.rds'))
gc()

# ==============================================================================
# F6 - INSURANCE AND PENSIONS
# ==============================================================================

# --- ECB QSA ---
insurancepensions_al=mds('ECB/QSA/Q.N..W0+W1...N.A+L.LE.F6._Z._Z.XDC._T.S.V.N._T')
saveRDS(insurancepensions_al, file.path(data_dir, 'insurancepensions_al.rds'))

# --- Eurostat BoP ---
bopf6q=mds('Estat/bop_iip6_q/Q.MIO_EUR.FA__O__F6..S1.A_LE+L_LE.WRL_REST.')
bopf6_a=mds('Estat/bop_iip6_q/A.MIO_EUR.FA__O__F6..S1.A_LE+L_LE.WRL_REST.')
saveRDS(bopf6q, file.path(data_dir, 'bopf6q.rds'))

bopf6a=copy(bopf6_a); frequency(bopf6a)='Q'
saveRDS(bopf6a, file.path(data_dir, 'bopf6a.rds'))

# --- Counterpart sector processing ---
names(dimnames(insurancepensions_al)) =gsub("^COUNTER.*$","COUNTERPART_SECTOR",names(dimnames(insurancepensions_al)))
insurancepensions_al=aperm(insurancepensions_al,c("REF_AREA","REF_SECTOR","COUNTERPART_SECTOR","ACCOUNTING_ENTRY","TIME"))

insurancepensions=insurancepensions_al[...A.]

dimnames(insurancepensions)$COUNTERPART_SECTOR = c(W0='S0' , W1='S2')[dimnames(insurancepensions)$COUNTERPART_SECTOR]
insurancepensions[.S2.S0.] =insurancepensions_al[.S1.W1.L.]
insurancepensions[.S2.S1.] =insurancepensions_al[.S1.W1.L.]
insurancepensions[.S1.S1.] = insurancepensions[.S1.S0.] -insurancepensions[.S1.S2.] 
insurancepensions[.S0.S1.] = insurancepensions[.S1.S1.] +insurancepensions[.S2.S1.] 
saveRDS(insurancepensions, file.path(data_dir, 'insurancepensions.rds'))
gc()
