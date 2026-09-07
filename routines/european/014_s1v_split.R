# ==============================================================================
# split_s1v.R
#
# Splits the combined household + non-financial corporation sector (S1V) in the
# BOP financial account into S11 and S1M, using the fact that the IIP reports
# the two sectors' stocks separately.
#
# Runs after aggregate_fin_sec.R. aggregate_fin_sec.R must be re-run afterwards
# so that _Tx7, _Tx8, F, Fx7 and Fx8 for S11 and S1M are rebuilt by summation.
#
# Decisions
#   Following Erza Aruqaj's outline, S1V_into_S11_and_S1M_-_flows_outline.pdf.
#   - Expert rules come first. Asset F2: the household leg is the change in the
#     household stock and the NFC leg is the residual. Asset F4, F81, F89 to
#     S11; asset F6 to S1M; asset F3, F5, F51, F511, F51M, F52 by the key.
#     Liability F3, F3S, F3L, F5, F51, F511, F51M, F52 to S11.
#   - Key: mean of LE(t-1..t-4), else LE(t-1), else LE(t). The computed leg is
#     rounded to 2 dp and the other leg is the residual, so S11 + S1M = S1V.
#   - Liability F4, F81 and F89 are added to S11, beyond her table. Validated on
#     WRL_REST: household share 0.80 per cent on F4, 0.04 on F81, 0.01 on F89,
#     with GR at 52.9 and MT at 20.1 the only outliers.
#   - The key does not fall back onto the F2 cells the deposit rule cannot fill.
#     Her text would allow it, her table gives F2 a rule, and the decisions
#     taken in this work do not allow it.
#   - The deposit rule uses reported stocks only, never a stock rebuilt by
#     summing member countries: reconstruction was designed for the key, where
#     it feeds a ratio, not for a level.
#   - STO = 'F' only. Stocks are never touched. S1V is left intact. onlyna on
#     every write. TIME_FLOOR 1999q4 on flows; stocks may reach earlier.
#   - Split at instrument x functional category level only. F, Fx7, Fx8, _T,
#     _Tx7 and _Tx8 are excluded and rebuilt afterwards.
#   - Tier resolved jointly, never mixed across sectors. The mean requires all
#     four preceding quarters, observed for both sectors, so the second tier is
#     reachable: a mean taken over whatever happens to be available would equal
#     the one-quarter lag whenever that lag is the only observation, and the
#     second tier would never be used. Usable only if both keys are
#     non-negative and their sum is positive. Missing is never zero. No
#     borrowing across instruments, functional categories or geographies.
#   - Geographic aggregate reconstruction on the issuer geography, reporter
#     excluded from its own aggregate, member coverage at least 0.8 required
#     separately for S11 and S1M. No EXT_ codes.
#   - The split is computed on the asset side and propagated to the liability
#     side, so the two agree by construction.
#   - Where one of S11 and S1M already carries a value and the other is NA,
#     nothing is written, on both sides. Her scheme assumes both legs are
#     written from scratch; the case arises only from onlyna, which is ours.
#     This is the one decision still open: writing the missing leg as S1V minus
#     the observed leg would fill more cells but discard the key there.
#   - The values are written through the as.data.table / as.md3 round trip.
#     A dense array over one of these slices would be hundreds of Gb.
#
# Unverified assumptions
#   - EU27 is taken to mean the post-Brexit composition.
#   - The member lists below are hard-coded.
# ==============================================================================

library(MDstats); library(MD3); library(data.table)

# Filled objects live in data/filled, while data_dir points at data/extracted
# in this session. The UNC form of this path does not resolve on the machine
# this runs on, so the mapped drive is used until the real host name is known.
filled_dir = 'Z:/FinFlows/githubrepo/data/filled'

aa=readRDS(file.path(filled_dir,'aa_iip_agg_fin.rds')); gc()
ll=readRDS(file.path(filled_dir,'ll_iip_agg_fin.rds')); gc()

TIME_FLOOR = '1999q4'
COV_THRESH = 0.8

RULE_AA = c(F4='S11', F81='S11', F89='S11', F6='S1M', F2='DELTA_STOCK',
            F3='KEY', F5='KEY', F51='KEY', F511='KEY', F51M='KEY', F52='KEY')

RULE_LL = c(F3='S11', F3S='S11', F3L='S11', F5='S11', F51='S11', F511='S11',
            F51M='S11', F52='S11', F4='S11', F81='S11', F89='S11')

INSTR_AGG = c('F','Fx7','Fx8')
FC_AGG    = c('_T','_Tx7','_Tx8')

AGGREGATES = list(
  EA18 = c('AT','BE','CY','DE','EE','ES','FI','FR','GR','EL','IE','IT','LU','LV','MT','NL','PT','SI','SK'),
  EA19 = c('AT','BE','CY','DE','EE','ES','FI','FR','GR','EL','IE','IT','LT','LU','LV','MT','NL','PT','SI','SK'),
  EA20 = c('AT','BE','CY','DE','EE','ES','FI','FR','GR','EL','HR','IE','IT','LT','LU','LV','MT','NL','PT','SI','SK'),
  EA21 = c('AT','BE','BG','CY','DE','EE','ES','FI','FR','GR','EL','HR','IE','IT','LT','LU','LV','MT','NL','PT','SI','SK'),
  EU27 = c('AT','BE','BG','CY','CZ','DE','DK','EE','ES','FI','FR','GR','EL','HR','HU','IE','IT','LT','LU','LV','MT','NL','PL','PT','RO','SE','SI','SK'),
  EU28 = c('AT','BE','BG','CY','CZ','DE','DK','EE','ES','FI','FR','GB','GR','EL','HR','HU','IE','IT','LT','LU','LV','MT','NL','PL','PT','RO','SE','SI','SK'))

KEY_AA   = c('INSTR','REF_AREA','COUNTERPART_SECTOR','FUNCTIONAL_CAT','COUNTERPART_AREA')
GEO_KEY  = c(KEY_AA,'TIME')
COLS_AA  = c('INSTR','REF_AREA','COUNTERPART_SECTOR','FUNCTIONAL_CAT','TIME','COUNTERPART_AREA')
COLS_LL  = c('INSTR','COUNTERPART_AREA','REF_SECTOR','FUNCTIONAL_CAT','TIME','REF_AREA')
COLS_LLR = c('INSTR','COUNTERPART_AREA','COUNTERPART_SECTOR','FUNCTIONAL_CAT','TIME','REF_AREA')
COLS_AAC = c('INSTR','REF_AREA','REF_SECTOR','FUNCTIONAL_CAT','TIME','COUNTERPART_AREA')

stopifnot(all(c('S11','S1M') %in% dimnames(aa)$REF_SECTOR),
          all(c('S11','S1M') %in% dimnames(aa)$COUNTERPART_SECTOR),
          all(c('S11','S1M') %in% dimnames(ll)$REF_SECTOR),
          all(c('S11','S1M') %in% dimnames(ll)$COUNTERPART_SECTOR))

# ---- asset-side flows to split -----------------------------------------------
# aa: INSTR . REF_AREA . REF_SECTOR . COUNTERPART_SECTOR . STO . FUNCTIONAL_CAT . TIME . COUNTERPART_AREA

s1vflo = unflag(aa['..S1V..F...']); gc()
flo = as.data.table(s1vflo, na.rm=TRUE)
setnames(flo, setdiff(names(flo), names(dimnames(s1vflo))), 'V')
flo[, TIME := as.character(TIME)]
stopifnot(all(grepl('^[0-9]{4}q[1-4]$', flo$TIME)))
flo = flo[is.finite(V) & TIME >= TIME_FLOOR]
flo[, rule := RULE_AA[INSTR]]
flo[INSTR %in% INSTR_AGG | FUNCTIONAL_CAT %in% FC_AGG, rule := NA_character_]
flo[, qi := 4L*as.integer(substr(TIME,1,4)) + as.integer(substr(TIME,6,6))]
flo[, sid := do.call(paste, c(.SD, sep='|')), .SDcols=KEY_AA]
rm(s1vflo); gc()

# ---- stocks ------------------------------------------------------------------

s1vsto = unflag(aa['..S11+S1M..LE...']); gc()
sto = as.data.table(s1vsto, na.rm=TRUE)
setnames(sto, setdiff(names(sto), names(dimnames(s1vsto))), 'V')
sto[, TIME := as.character(TIME)]
stopifnot(all(grepl('^[0-9]{4}q[1-4]$', sto$TIME)))
sto = sto[is.finite(V) & !INSTR %in% INSTR_AGG & !FUNCTIONAL_CAT %in% FC_AGG]
rm(s1vsto); gc()

wide = dcast(sto, INSTR + REF_AREA + COUNTERPART_SECTOR + FUNCTIONAL_CAT +
               COUNTERPART_AREA + TIME ~ REF_SECTOR, value.var='V')
if (!'S11' %in% names(wide)) wide[, S11 := NA_real_]
if (!'S1M' %in% names(wide)) wide[, S1M := NA_real_]
wide[, src := 'reported']

# ---- geographic aggregate reconstruction, issuer geography -------------------
# The reporter is excluded from its own aggregate and at least COV_THRESH of the
# remaining members must be observed, separately for each sector.

rec = rbindlist(lapply(names(AGGREGATES), function(agg) {
  mem = AGGREGATES[[agg]]
  m = wide[src=='reported' & COUNTERPART_AREA %in% mem & REF_AREA != COUNTERPART_AREA]
  if (!nrow(m)) return(NULL)
  r = m[, .(S11=sum(S11, na.rm=TRUE), n11=sum(is.finite(S11)),
            S1M=sum(S1M, na.rm=TRUE), n1M=sum(is.finite(S1M))),
        by=.(INSTR, REF_AREA, COUNTERPART_SECTOR, FUNCTIONAL_CAT, TIME)]
  r[, n_members := length(setdiff(mem, REF_AREA)), by=REF_AREA]
  r = r[n11/n_members >= COV_THRESH & n1M/n_members >= COV_THRESH]
  if (!nrow(r)) return(NULL)
  r[, `:=`(COUNTERPART_AREA=agg, src='reconstructed')]
  r[, c(GEO_KEY,'S11','S1M','src'), with=FALSE]
}))

if (nrow(rec)) {
  rec = rec[!wide[, ..GEO_KEY], on=GEO_KEY]
  wide = rbind(wide[, c(GEO_KEY,'S11','S1M','src'), with=FALSE], rec)
}

wide[, qi := 4L*as.integer(substr(TIME,1,4)) + as.integer(substr(TIME,6,6))]
wide[, sid := do.call(paste, c(.SD, sep='|')), .SDcols=KEY_AA]

# ---- statistical key ---------------------------------------------------------
# Only quarters where both sectors are observed enter, and all four are
# required, so the moving average and the one-quarter lag stay distinct tiers.
# The tier is the highest one available for both sectors.

both = wide[is.finite(S11) & is.finite(S1M), .(sid, qi, S11, S1M)]

t1 = rbindlist(lapply(1:4, function(L) both[, .(sid, qi=qi+L, S11, S1M)]))
t1 = t1[, .(k11=mean(S11), k1M=mean(S1M), nq=.N), by=.(sid, qi)]
t1 = t1[nq==4L][, nq := NULL][, tier := 1L]
t2 = both[, .(sid, qi=qi+1L, k11=S11, k1M=S1M)][, tier := 2L]
t3 = both[, .(sid, qi, k11=S11, k1M=S1M)][, tier := 3L]

key = rbindlist(list(t1,t2,t3), use.names=TRUE)
setorder(key, sid, qi, tier)
key = key[, .SD[1], by=.(sid, qi)]
key = key[is.finite(k11) & is.finite(k1M) & k11 >= 0 & k1M >= 0 & (k11+k1M) > 0]
key[, share_S11 := k11/(k11+k1M)]

# ---- household stock change, for the deposit rule ----------------------------
# Reported stocks only, and the contemporaneous difference, which is the
# identification assumption of the rule rather than a weight.

hh = wide[src=='reported' & is.finite(S1M), .(sid, qi, S1M)]
hh = merge(hh, hh[, .(sid, qi=qi+1L, S1M_prev=S1M)], by=c('sid','qi'))
hh[, dS1M := S1M - S1M_prev]

# ---- allocation --------------------------------------------------------------

flo = merge(flo, key[, .(sid, qi, share_S11)], by=c('sid','qi'), all.x=TRUE)
flo = merge(flo, hh[, .(sid, qi, dS1M)], by=c('sid','qi'), all.x=TRUE)

flo[, `:=`(F_S11=NA_real_, F_S1M=NA_real_)]

flo[rule=='S11', `:=`(F_S11=round(V,2), F_S1M=0)]
flo[rule=='S1M', `:=`(F_S11=0, F_S1M=round(V,2))]
flo[rule=='KEY' & is.finite(share_S11), F_S11 := round(V*share_S11, 2)]
flo[rule=='KEY' & is.finite(share_S11), F_S1M := round(V,2) - F_S11]
flo[rule=='DELTA_STOCK' & is.finite(dS1M), F_S1M := round(dS1M, 2)]
flo[rule=='DELTA_STOCK' & is.finite(dS1M), F_S11 := round(V,2) - F_S1M]

res = flo[is.finite(F_S11) & is.finite(F_S1M)]
stopifnot(max(abs(round(res$V,2) - res$F_S11 - res$F_S1M)) < 1e-6)

# ---- cells where one leg already exists are left alone -----------------------

exflo = unflag(aa['..S11+S1M..F...']); gc()
ex = as.data.table(exflo, na.rm=TRUE)
setnames(ex, setdiff(names(ex), names(dimnames(exflo))), 'V')
ex[, TIME := as.character(TIME)]
ex = ex[is.finite(V) & TIME >= TIME_FLOOR]
ex = dcast(ex, INSTR + REF_AREA + COUNTERPART_SECTOR + FUNCTIONAL_CAT +
             COUNTERPART_AREA + TIME ~ REF_SECTOR, value.var='V')
if (!'S11' %in% names(ex)) ex[, S11 := NA_real_]
if (!'S1M' %in% names(ex)) ex[, S1M := NA_real_]
setnames(ex, c('S11','S1M'), c('have_S11','have_S1M'))
rm(exflo); gc()

res = merge(res, ex, by=GEO_KEY, all.x=TRUE)
towrite = res[!is.finite(have_S11) & !is.finite(have_S1M)]

# ---- write the asset side ----------------------------------------------------
# The slice keeps INSTR, REF_AREA, COUNTERPART_SECTOR, FUNCTIONAL_CAT, TIME and
# COUNTERPART_AREA. The existing rows are read out, the new rows appended, and
# the object rebuilt with as.md3, as in 001_prep_aalldomestic.R.

tgt = unflag(aa['..S11..F...'])
d11 = as.data.table(tgt, na.rm=TRUE)
setnames(d11, setdiff(names(d11), names(dimnames(tgt))), 'obs_value')
d11[, TIME := as.character(TIME)]
d11 = rbind(d11, towrite[, c(COLS_AA, 'F_S11'), with=FALSE], use.names=FALSE)
attr(d11, 'dcstruct') = MD3:::.dimcodesrescue(dimnames(tgt), dimcodes(tgt))
attr(d11, 'dcsimp')   = dimnames(tgt)
aa['..S11..F...', usenames=TRUE] = as.md3(d11)
rm(tgt, d11); gc()

tgt = unflag(aa['..S1M..F...'])
d1M = as.data.table(tgt, na.rm=TRUE)
setnames(d1M, setdiff(names(d1M), names(dimnames(tgt))), 'obs_value')
d1M[, TIME := as.character(TIME)]
d1M = rbind(d1M, towrite[, c(COLS_AA, 'F_S1M'), with=FALSE], use.names=FALSE)
attr(d1M, 'dcstruct') = MD3:::.dimcodesrescue(dimnames(tgt), dimcodes(tgt))
attr(d1M, 'dcsimp')   = dimnames(tgt)
aa['..S1M..F...', usenames=TRUE] = as.md3(d1M)
rm(tgt, d1M); gc()

# ---- propagate to the liability side -----------------------------------------
# aa REF_SECTOR = S1V and ll COUNTERPART_SECTOR = S1V are the same facts with
# the geographies swapped. Propagating rather than recomputing is what makes the
# two sides agree.
# ll: INSTR . COUNTERPART_AREA . COUNTERPART_SECTOR . REF_SECTOR . STO . FUNCTIONAL_CAT . TIME . REF_AREA

mir = copy(towrite)
setnames(mir, 'COUNTERPART_SECTOR', 'REF_SECTOR')
setnames(mir, 'REF_AREA', 'TMP_AREA')
setnames(mir, 'COUNTERPART_AREA', 'REF_AREA')
setnames(mir, 'TMP_AREA', 'COUNTERPART_AREA')
gc()
tgt = unflag(ll['..S11..F...'])
d11 = as.data.table(tgt, na.rm=TRUE)
setnames(d11, setdiff(names(d11), names(dimnames(tgt))), 'obs_value')
d11[, TIME := as.character(TIME)]
d11 = rbind(d11, mir[, c(COLS_LL, 'F_S11'), with=FALSE], use.names=FALSE)
attr(d11, 'dcstruct') = MD3:::.dimcodesrescue(dimnames(tgt), dimcodes(tgt))
attr(d11, 'dcsimp')   = dimnames(tgt)
gc()
ll['..S11..F...', usenames=TRUE] = as.md3(d11)
rm(tgt, d11); gc()

tgt = unflag(ll['..S1M..F...'])
d1M = as.data.table(tgt, na.rm=TRUE)
setnames(d1M, setdiff(names(d1M), names(dimnames(tgt))), 'obs_value')
d1M[, TIME := as.character(TIME)]
d1M = rbind(d1M, mir[, c(COLS_LL, 'F_S1M'), with=FALSE], use.names=FALSE)
attr(d1M, 'dcstruct') = MD3:::.dimcodesrescue(dimnames(tgt), dimcodes(tgt))
attr(d1M, 'dcsimp')   = dimnames(tgt)
ll['..S1M..F...', usenames=TRUE] = as.md3(d1M)
rm(tgt, d1M); gc()

# ---- liability side, everything to S11 ---------------------------------------

s1vlflo = unflag(ll['...S1V.F...']); gc()
lflo = as.data.table(s1vlflo, na.rm=TRUE)
setnames(lflo, setdiff(names(lflo), names(dimnames(s1vlflo))), 'V')
lflo[, TIME := as.character(TIME)]
stopifnot(all(grepl('^[0-9]{4}q[1-4]$', lflo$TIME)))
lflo = lflo[is.finite(V) & TIME >= TIME_FLOOR]
lflo[, rule := RULE_LL[INSTR]]
lflo[INSTR %in% INSTR_AGG | FUNCTIONAL_CAT %in% FC_AGG, rule := NA_character_]
lflo = lflo[rule=='S11']
lflo[, `:=`(F_S11=round(V,2), F_S1M=0)]
rm(s1vlflo); gc()

# Same guard as on the asset side: where one of the two legs already carries a
# value, nothing is written, otherwise the pair would not add back to S1V.

lex = unflag(ll['...S11+S1M.F...']); gc()
lhave = as.data.table(lex, na.rm=TRUE)
setnames(lhave, setdiff(names(lhave), names(dimnames(lex))), 'V')
lhave[, TIME := as.character(TIME)]
lhave = lhave[is.finite(V) & TIME >= TIME_FLOOR]
lhave = dcast(lhave, INSTR + COUNTERPART_AREA + COUNTERPART_SECTOR + FUNCTIONAL_CAT +
                TIME + REF_AREA ~ REF_SECTOR, value.var='V')
if (!'S11' %in% names(lhave)) lhave[, S11 := NA_real_]
if (!'S1M' %in% names(lhave)) lhave[, S1M := NA_real_]
setnames(lhave, c('S11','S1M'), c('had_S11','had_S1M'))
rm(lex); gc()

lflo = merge(lflo, lhave, by=COLS_LLR, all.x=TRUE)
lflo = lflo[!is.finite(had_S11) & !is.finite(had_S1M)]

tgt = unflag(ll['...S11.F...'])
d11 = as.data.table(tgt, na.rm=TRUE)
setnames(d11, setdiff(names(d11), names(dimnames(tgt))), 'obs_value')
d11[, TIME := as.character(TIME)]
d11 = rbind(d11, lflo[, c(COLS_LLR, 'F_S11'), with=FALSE], use.names=FALSE)
attr(d11, 'dcstruct') = MD3:::.dimcodesrescue(dimnames(tgt), dimcodes(tgt))
attr(d11, 'dcsimp')   = dimnames(tgt)
ll['...S11.F...', usenames=TRUE] = as.md3(d11)
rm(tgt, d11); gc()

tgt = unflag(ll['...S1M.F...'])
d1M = as.data.table(tgt, na.rm=TRUE)
setnames(d1M, setdiff(names(d1M), names(dimnames(tgt))), 'obs_value')
d1M[, TIME := as.character(TIME)]
d1M = rbind(d1M, lflo[, c(COLS_LLR, 'F_S1M'), with=FALSE], use.names=FALSE)
attr(d1M, 'dcstruct') = MD3:::.dimcodesrescue(dimnames(tgt), dimcodes(tgt))
attr(d1M, 'dcsimp')   = dimnames(tgt)
ll['...S1M.F...', usenames=TRUE] = as.md3(d1M)
rm(tgt, d1M); gc()

# ---- and its mirror on the asset side ----------------------------------------

lmir = copy(lflo)
setnames(lmir, 'COUNTERPART_SECTOR', 'REF_SECTOR')
setnames(lmir, 'REF_AREA', 'TMP_AREA')
setnames(lmir, 'COUNTERPART_AREA', 'REF_AREA')
setnames(lmir, 'TMP_AREA', 'COUNTERPART_AREA')

tgt = unflag(aa['...S11.F...'])
d11 = as.data.table(tgt, na.rm=TRUE)
setnames(d11, setdiff(names(d11), names(dimnames(tgt))), 'obs_value')
d11[, TIME := as.character(TIME)]
d11 = rbind(d11, lmir[, c(COLS_AAC, 'F_S11'), with=FALSE], use.names=FALSE)
attr(d11, 'dcstruct') = MD3:::.dimcodesrescue(dimnames(tgt), dimcodes(tgt))
attr(d11, 'dcsimp')   = dimnames(tgt)
aa['...S11.F...', usenames=TRUE] = as.md3(d11)
rm(tgt, d11); gc()

tgt = unflag(aa['...S1M.F...'])
d1M = as.data.table(tgt, na.rm=TRUE)
setnames(d1M, setdiff(names(d1M), names(dimnames(tgt))), 'obs_value')
d1M[, TIME := as.character(TIME)]
d1M = rbind(d1M, lmir[, c(COLS_AAC, 'F_S1M'), with=FALSE], use.names=FALSE)
attr(d1M, 'dcstruct') = MD3:::.dimcodesrescue(dimnames(tgt), dimcodes(tgt))
attr(d1M, 'dcsimp')   = dimnames(tgt)
aa['...S1M.F...', usenames=TRUE] = as.md3(d1M)
rm(tgt, d1M); gc()

saveRDS(aa, file.path(filled_dir,'aa_s1v_split.rds'))
saveRDS(ll, file.path(filled_dir,'ll_s1v_split.rds'))

saveRDS(aa, file.path(filled_dir,'vintages/aa_s1v_split_' %&% format(Sys.time(),'%F') %&% '_.rds'))
saveRDS(ll, file.path(filled_dir,'vintages/ll_s1v_split_' %&% format(Sys.time(),'%F') %&% '_.rds'))