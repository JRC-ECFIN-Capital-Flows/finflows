library(MDstats)
library(MD3)
library(data.table)

# Set data directory
#data_dir= file.path(getwd(),'data')
if (!exists("data_dir")) data_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/filled'
source('\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/finflows/routines/utilities.R')
gc()


## load filled iip bop
aa=readRDS(file.path(data_dir,'aa_iip_lbs.rds')); gc()
ll=readRDS(file.path(data_dir,'ll_iip_lbs.rds')); gc()
gc()

######### This script calculates a data quality score for each country-instrument-functional category combination 
######### These scores are useful when the position of country A to country B is different depending on whether it is reported by A or B
######### Based on the scores, the final position is calculated as a weighted average of the two data sources
######### The final score itself is a weighted average of two other scores: Input A and Input B.
######### The weighting can be modified arbitrarily:
alpha=0.5 
######### alpha should take values between 0 and 1, where higher alpha means input A receives a larger weight

####### Input A: Average Bilateral Discrepancy Score ##########

## need to rename ll dimensions temporarily
## Both `-` and `[<-` in MD3 align BY NAME. To pair the SAME physical cell
## (asset-side vs liability-side of one bilateral position) we must relabel ll
## so its names equal aa's on the physically-corresponding axis. That requires
## swapping BOTH area dims (2<->8) AND sector dims (3<->4): after this block ll
## carries aa's labelling exactly (REF_=holder, COUNTERPART_=issuer).

# areas: pos2 (COUNTERPART_AREA) <-> pos8 (REF_AREA)
names(dimnames(ll))[2] = 'TEMP_AREA'
names(dimnames(ll))[8] = 'COUNTERPART_AREA'
names(dimnames(ll))[2] = 'REF_AREA'
# sectors: pos3 (COUNTERPART_SECTOR) <-> pos4 (REF_SECTOR)
names(dimnames(ll))[3] = 'TEMP_SECTOR'
names(dimnames(ll))[4] = 'COUNTERPART_SECTOR'
names(dimnames(ll))[3] = 'REF_SECTOR'

disc <- aa["......."] - ll["......."] # create matrix of discrepancies (differences between an Asset position and its corresponding Liability counterpart)

## Convert to data.table to ease handling and filter out NAs

disc <- as.data.table(disc, .simple=TRUE); gc()
disc <- disc[!is.nan(obs_value) & !is.infinite(obs_value) & !is.na(obs_value)]

# Specify set of countries, instruments, and functional categories across which we would like to calculate scores

ra_vec <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","EA20","EU27")
fi_vec <- c("F","F2M","F3","F3S","F3L","F4","F4S","F4L","F5","F51","F511","F51M","F52","F6","F81","F89","Fx7")
fc_vec <- c("_T","_P","_D","_O","_R")

## Calculate the average value of discrepancies for each of the specified country-instrument-functional category combination

mean_disc <- disc[REF_AREA %in% ra_vec & INSTR %in% fi_vec & FUNCTIONAL_CAT %in% fc_vec, 
                  .(mean_disc = mean(abs(obs_value))), 
                  by = .(REF_AREA, INSTR, FUNCTIONAL_CAT)]

# NAs appear if only NA values are present for an AREA,INSTR, FC combination
# this is normal, some FC and INSTR combinations never reported
# Note that COUNTEPART_AREA is not specified, so discrepancies are not just calculated over other EU counterparts
# latter can be changed by adding an extra filter to line 48: "& COUNTERPART_AREA %in% ra_vec"

## Normalisation - scores ranked across 0 to 1 interval - higher score means smaller discrepancies

mean_disc[, mean_disc := 1 - (mean_disc - min(mean_disc)) / (max(mean_disc) - min(mean_disc))]
# distribution really skewed due to EA, and EU aggregates have very large discrepancies - liabilities missing?

gc()

# reset old naming for next part 
# invert BOTH swaps to restore ll's native labelling

# areas back: pos8 -> REF_AREA, pos2 -> COUNTERPART_AREA
names(dimnames(ll))[8] = 'TEMP_AREA'
names(dimnames(ll))[2] = 'COUNTERPART_AREA'
names(dimnames(ll))[8] = 'REF_AREA'
# sectors back: pos4 -> REF_SECTOR, pos3 -> COUNTERPART_SECTOR
names(dimnames(ll))[4] = 'TEMP_SECTOR'
names(dimnames(ll))[3] = 'COUNTERPART_SECTOR'
names(dimnames(ll))[4] = 'REF_SECTOR'

####### Input B: Completeness Score (Ratio of NAs) #########

fi_string <- paste(fi_vec, collapse="+")
ra_string <- paste(ra_vec, collapse="+")
# Again, keep all counterpart areas, except aggregates
ca_vec <- dimnames(aa)$COUNTERPART_AREA
ca_string <- paste(setdiff(ca_vec, c("W2", "WRL_REST", "W0")), collapse = "+")

# Filter out NAs
daa <- as.data.table(aa[paste0(fi_string,".",ra_string,"...._T+_P+_D+_O+_R..",ca_string), drop = FALSE], .simple = TRUE)
dll <- as.data.table(ll[paste0(fi_string,".",ca_string,"...._T+_P+_D+_O+_R..",ra_string), drop = FALSE], .simple = TRUE)
daa <- daa[!is.nan(obs_value) & !is.infinite(obs_value) & !is.na(obs_value)]
dll <- dll[!is.nan(obs_value) & !is.infinite(obs_value) & !is.na(obs_value)]
# Again, if we want to calculate the NA ratio only over EU countries, switch ca_string to ra_string in line 78, 79

### We calculate ratio of NAs as: 1 minus ratio of non-NAs

## Calculate ratio of non-NAs

# Calculate numerator (number of non-NAs) for each specified country-instrument-functional category combination

non_NAs_aa <- daa[, .(n = .N),
                  by = .(REF_AREA, INSTR, FUNCTIONAL_CAT)]
non_NAs_ll <- dll[, .(n = .N),
                  by = .(REF_AREA, INSTR, FUNCTIONAL_CAT)]

non_NAs <- merge(non_NAs_aa, non_NAs_ll, by = c("REF_AREA", "INSTR", "FUNCTIONAL_CAT"))
non_NAs <- non_NAs[, .(REF_AREA, INSTR, FUNCTIONAL_CAT,
                       non_NAs = n.x + n.y)]

# Calculate denominator (totals = NAs + non NAs)  for each specified country-instrument-functional category combination

N_aa <- as.numeric(length(dimnames(aa)$REF_SECTOR)*length(dimnames(aa)$COUNTERPART_SECTOR)*length(dimnames(aa)$STO)*length(dimnames(aa)$TIME)*length(dimnames(aa)$COUNTERPART_AREA))
N_ll <- as.numeric(length(dimnames(ll)$REF_SECTOR)*length(dimnames(ll)$COUNTERPART_SECTOR)*length(dimnames(ll)$STO)*length(dimnames(ll)$TIME)*length(dimnames(ll)$COUNTERPART_AREA))

N <- N_aa+N_ll

## Calculate Ratio of NAs

na_shares <- non_NAs[, .(REF_AREA, INSTR, FUNCTIONAL_CAT,
                         na_share = 1-(non_NAs/N))]

## Normalisation - scores ranked across 0 to 1 interval - higher score means less NAs

na_shares[, na_share := 1 - (na_share - min(na_share)) / (max(na_share) - min(na_share))]

####### Combine A & B #########

score <- merge(mean_disc,na_shares,by=c("REF_AREA", "INSTR", "FUNCTIONAL_CAT"))
## calculate final score using previously specified weight
score  <- score[, score := ifelse(is.na(mean_disc)==TRUE,na_share,alpha*mean_disc + (1-alpha)*na_share)]
score <- score[,c("REF_AREA", "INSTR", "FUNCTIONAL_CAT","score")]
## create database of scores for each counterpart area as well
score_CA <- score
score_CA  <- score_CA[, COUNTERPART_AREA := REF_AREA]
score <- score[,c("REF_AREA", "INSTR", "FUNCTIONAL_CAT","score")]
score_CA <- score_CA[,c("COUNTERPART_AREA", "INSTR", "FUNCTIONAL_CAT","score")]


# create a relative score for each country pair 
score_pairs <- merge(score, score_CA, by = c("INSTR", "FUNCTIONAL_CAT"), allow.cartesian = TRUE)
score_pairs  <- score_pairs[, score:=score.x/(score.x+score.y)]
score_pairs  <- score_pairs[, c("REF_AREA","COUNTERPART_AREA", "INSTR", "FUNCTIONAL_CAT","score")]

score_pairs <- as.md3(score_pairs)


##### Start correction #####

## Same alignment as above: swap BOTH areas (2<->8) and sectors (3<->4) so that
## every by-name operation below (disc + ll, the score-weighted writes, and the
## ll<->aa fills) pairs matching physical cells rather than transposing sectors.

# areas: pos2 (COUNTERPART_AREA) <-> pos8 (REF_AREA)
names(dimnames(ll))[2] = 'TEMP_AREA'
names(dimnames(ll))[8] = 'COUNTERPART_AREA'
names(dimnames(ll))[2] = 'REF_AREA'
# sectors: pos3 (COUNTERPART_SECTOR) <-> pos4 (REF_SECTOR)
names(dimnames(ll))[3] = 'TEMP_SECTOR'
names(dimnames(ll))[4] = 'COUNTERPART_SECTOR'
names(dimnames(ll))[3] = 'REF_SECTOR'

## Assets ##
# When both asset and liability is available, take score-weighted average


# Note that we only want to fill rows where both A and L present 
# otherwise, if A present and L not, A will be filled NA (score-weighted average doesn't exist)
disc <- disc[obs_value != 0]
disc <- as.md3(disc)

# Obtain asset data, filtered for rows where discrepancies exist
aa_new <- disc["......."] + ll["......."]

##### Main contribution #####

# calculate re-weighted assets using the pair-specific, relative weights we calculated before
aa_new[paste0(".......",ca_string),usenames=TRUE] <- aa[paste0(".......",ca_string)]*(score_pairs) +
  ll[paste0(".",ca_string,"......")]*(1-score_pairs)

## check - aa_new should be between aa and ll
aa["F3.AT.S1.S1.LE._P.2022q4.IT"]
aa_new["F3.AT.S1.S1.LE._P.2022q4.IT"]
ll["F3.AT.S1.S1.LE._P.2022q4.IT"]

# fill in (replace) assets with re-weighted assets, N should not change
# usenames=TRUE (as in the fills below) writes aa_new's cells into aa MATCHED BY
# CODE: the score-weighted average lands where both A & L exist (aa_new present),
# and aa is left untouched where aa_new is absent (A-only cells keep A). Line 172
# then supplies the L-only cells. Do NOT use a plain positional assignment here:
# `aa["."] <- aa_new["."]` recycles/broadcasts the RHS across the selection when
# extents differ, rather than matching cell-by-cell.
aa[".......", usenames=TRUE] <- aa_new["......."]

##### Important step #####

# When asset data missing, take liability data (if available)
aa[paste0(".......",ca_string), usenames=TRUE, onlyna=TRUE] <- ll[paste0(".",ca_string,"......")]

## Liabilities ##

# check (pre) if two are equal
# ll["F3.AT.S1.S1.LE._P.2022q4.IT"]
# aa["F3.AT.S1.S1.LE._P.2022q4.IT"]
# aa["F4.AT.S1.S1.LE..2022q4.US"]
# ll["F4.AT.S1.S1.LE..2022q4.US"]

## For liabilities we just take the recalculated asset data

# If L is NA, but A isn't, then L is set to A (new value) - good
# If L non-NA, but A is NA, then L in theory would be set to NA - this wont happen, as all NA assets were set to L
# If both L, and A is non-NA, then L is set as the new weighted value - good
ll[paste0(".",ca_string,"......"), usenames=TRUE] <- aa[paste0(".......",ca_string)]

# check (post) if two are equal (should be)
# ll["F3.AT.S1.S1.LE._P.2022q4.IT"]
# aa["F3.AT.S1.S1.LE._P.2022q4.IT"]
# aa["F4.AT.S1.S1.LE..2022q4.US"]
# ll["F4.AT.S1.S1.LE..2022q4.US"]

## reset ll to native labelling before saving (invert both swaps)

# areas back
names(dimnames(ll))[8] = 'TEMP_AREA'
names(dimnames(ll))[2] = 'COUNTERPART_AREA'
names(dimnames(ll))[8] = 'REF_AREA'
# sectors back
names(dimnames(ll))[4] = 'TEMP_SECTOR'
names(dimnames(ll))[3] = 'COUNTERPART_SECTOR'
names(dimnames(ll))[4] = 'REF_SECTOR'

obs <- as.data.table(aa, .simple=TRUE); gc()

aa=saveRDS(aa,file.path(data_dir,'aa_iip_score.rds')); gc()
ll=saveRDS(ll,file.path(data_dir,'ll_iip_score.rds')); gc()