# Load required packages
library(MDstats)
library(MD3)

# Set data directory and country codes
if (!exists("data_dir")) data_dir = getwd()
defaultcountrycode(NULL)

loaddata=!file.exists(file.path(data_dir, 'govf5src.rds'))

if (loaddata) {
  govqsa=mds('ECB/QSA/Q.N..W2+W0+W1.S13..N.A+L.LE.F5+F511+F51M+F52._Z._Z.XDC._T.S.V.N._T', labels=TRUE)
  govqsl=mds('ECB/QSA/Q.N..W2+W0+W1..S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T', labels=FALSE)
  goviipq=mds('ESTAT/bop_iip6_q/Q.MIO_EUR.FA__D__F5+FA__P__F5+FA__O__F519.S13.S1.A_LE+L_LE.WRL_REST.', labels=TRUE)
  goviipa=mds('ESTAT/bop_iip6_q/A.MIO_EUR.FA__D__F5+FA__P__F5+FA__O__F519.S13.S1.A_LE+L_LE.WRL_REST.', labels=TRUE)
  goveaiipass=mds('ESTAT/bop_eu6_q/Q.EA19+EA20+EA21.EXT_EA19+EXT_EA20+EXT_EA21.S1.S13.ASS.NSA.FA__D__F51M+FA__O__F519.', labels=TRUE)
  goveaiipliab=mds('ESTAT/bop_eu6_q/EA19+EA20+EA21.EXT_EA19+EXT_EA20+EXT_EA21.S1.S13.LIAB.NSA.FA__D__F51M+FA__O__F519.', labels=TRUE)
  gov5raw=mds('ESTAT/gov_10q_ggfa/Q.F5..STK...NCO+CO.MIO_EUR.', labels=TRUE)
  govcp=mds('ESTAT/nasa_10_f_cp/A.STK..S13.F5.ASS.MIO_EUR.')
  xrraw=mds('Eurostat/NASQ_10_F_BS/Q.MIO_EUR+MIO_NAC.S13.LIAB.F3.')
  saveRDS(list(gov5raw=gov5raw,goviipq=goviipq,goviipa=goviipa,govqsa=govqsa, govqsl=govqsl, xrraw=xrraw, govcp=govcp), file=file.path(data_dir, 'govf5src.rds'))
} else {
  lll=readRDS(file.path(data_dir, 'govf5src.rds')); gov5raw=lll$gov5; goviipq=lll$goviipq; goviipa=lll$goviipa; govqsa=lll$govqsa; govqsl=lll$govqsl; govcp=lll$govcp; xrraw=lll$xrraw; rm(lll)
}

xr=xrraw[.MIO_NAC.]/xrraw[.MIO_EUR.]; names(dimnames(xr))[1]='REF_AREA'

govqsa = suppressWarnings(govqsa/xr)
dimnames(govqsa)[['REF_AREA']] =ccode(dimnames(govqsa)[['REF_AREA']],2,'iso2m', leaveifNA = TRUE)

dimnames(goviipq)[['geo']] =ccode(dimnames(goviipq)[['geo']],2,'iso2m', leaveifNA = TRUE)
goviipq[..F5.]= goviipq[..FA__D__F5.] + goviipq[..FA__P__F5.] + goviipq[..FA__O__F519.]
goviipq[..F5.,onlyna=TRUE]=  goviipq[..FA__P__F5.] + goviipq[..FA__O__F519.]
goviipq[..F5.,onlyna=TRUE]=  goviipq[..FA__P__F5.] + goviipq[..FA__D__F519.]
goviipq[.L_LE.F5.,onlyna=TRUE] = 0
names(dimnames(goviipq))[names(dimnames(goviipq))=='stk_flow']='finpos'

dimnames(goviipa)[['geo']] =ccode(dimnames(goviipa)[['geo']],2,'iso2m', leaveifNA = TRUE)

goviipaq=copy(goviipa)
frequency(goviipaq) = 'Q'
names(dimnames(goviipaq))[names(dimnames(goviipaq))=='stk_flow']='finpos'

goviipq[..F5.]= goviipaq[..FA__D__F5.] + goviipaq[..FA__P__F5.] + goviipaq[..FA__O__F519.]
goviipq[..F5.,onlyna=TRUE]=  goviipaq[..FA__P__F5.] + goviipaq[..FA__O__F519.]
goviipq[..F5.,onlyna=TRUE]=  goviipaq[..FA__P__F5.] + goviipaq[..FA__D__F519.]

gov5=gov5raw
dimnames(gov5)[['sector2']]=gsub('S1_S2','S0',dimnames(gov5)[['sector2']])
names(dimnames(gov5))=gsub('sector2$','COUNTERPART_SECTOR',names(dimnames(gov5)))
dimnames(gov5)[['geo']] =ccode(dimnames(gov5)[['geo']],2,'iso2m', leaveifNA = TRUE)
dimnames(gov5)[['finpos']]=gsub('^L.*','L_LE',gsub('^A.*','A_LE',dimnames(gov5)[['finpos']]))

gov5[..S13..NCO., onlyna=TRUE] = gov5[..S0..NCO.]-gov5[..S0..CO.]

gov5[A_LE.MT+GR+RO+BG.S2.S1314.NCO.,onlyna=TRUE] = 0

gov5[..S2.S13pseudo.NCO.]=gov5[..S2.S1311.NCO.]+gov5[..S2.S1314.NCO.]
gov5[..S2.S13pseudo.NCO.,justval=TRUE]=gov5[..S2.S13pseudo.NCO.]+gov5[..S2.S1313.NCO.]
gov5[..S2.S13pseudo.NCO.,justval=TRUE]=gov5[..S2.S13pseudo.NCO.]+gov5[..S2.S1312.NCO.]
gov5[,,'S2','S13pseudo','NCO',][is.na(gov5[,,'S2','S13','NCO',]) & is.na(gov5[,,'S2','S1311','NCO',])] = NA

gov5[..S2.S13F5.NCO.] = goviipq['..F5.y1999q1:']
gov5[..S2.S13pseudo.NCO.][which(gov5[..S2.S13pseudo.NCO.]<gov5[..S2.S13F5.NCO.])] = gov5[..S2.S13F5.NCO.][which(gov5[..S2.S13pseudo.NCO.]<gov5[..S2.S13F5.NCO.])]
gov5[..S2.S13pseudo.NCO.,onlyna=TRUE] = gov5[..S2.S13F5.NCO.]

gov5[..S2.S13.NCO.,onlyna=TRUE]  =gov5[..S2.S13pseudo.NCO.]
gov5[..S13.S13.NCO.,onlyna=TRUE]=gov5[..S0.S13.NCO.]-gov5[..S0.S13.CO.]

gov5[..S14_S15.S13.NCO.,onlyna=TRUE] = gov5[..S14_S15.S1311.NCO.] +gov5[..S14_S15.S1313.NCO.] + gov5[..S14_S15.S1314.NCO.]
gov5[..S14_S15.S13.NCO.,onlyna=TRUE] = gov5[..S14_S15.S1311.NCO.] + gov5[..S14_S15.S1314.NCO.]
gov5[..S14_S15.S13.NCO.,onlyna=TRUE] = gov5[..S14_S15.S1311.NCO.]
gov5[A_LE..S14_S15.S13.NCO.,onlyna=TRUE] = 0

gov5[..S128_S129.S13.NCO.,onlyna=TRUE] = gov5[..S128_S129.S1311.NCO.]+gov5[..S128_S129.S1314.NCO.]
gov5[..S128_S129.S13.NCO.,onlyna=TRUE] = gov5[..S128_S129.S1311.NCO.]

gov5[..S12.S13.NCO.,onlyna=TRUE] = gov5[..S12.S1311.NCO.]+gov5[..S12.S1314.NCO.]
gov5[..S12.S13.NCO.,onlyna=TRUE] = gov5[..S12.S1311.NCO.]
# note S12 negative for Spain between 2022q3 and 2024q2


gov5[..S11.S13.NCO.,onlyna=TRUE]   = gov5[..S0.S13.NCO.] - gov5[..S2.S13.NCO.] - gov5[..S14_S15.S13.NCO.] - gov5[..S13.S13.NCO.] - gov5[..S12.S13.NCO.]

goveq=copy(gov5[...S13.NCO.])
names(dimnames(goveq))=gsub('^geo','REF_AREA',gsub('^finpos','ACCOUNTING_ENTRY',names(dimnames(goveq))))
goveq=add.dim(goveq,'INSTR_ASSET',.dimcodes = dimnames(govqsa)$INSTR_ASSET,.fillall = FALSE)
dimnames(goveq)$COUNTERPART_SECTOR = gsub('S14_S15','S1M', gsub('S128_S129','S12Q', dimnames(goveq)$COUNTERPART_SECTOR))

goveq[F511+F52.A_LE..., usenames=TRUE] = govqsa[.W2..A.F511+F52.]
goveq[F511+F52.A_LE..S2., usenames=TRUE] = govqsa[.W1.S1.A.F511+F52.]
goveq[...S1., onlyna=TRUE] = goveq[...S0.] - goveq[...S2.]
goveq[...S0., onlyna=TRUE] = goveq[...S2.] + goveq[...S1.]
goveq[F52.A_LE..S11+S13+S1M+S12Q., onlyna=TRUE] =0
ix=which(goveq[F511.A_LE...]+goveq[F52.A_LE...] > goveq[F5.A_LE...])
goveq[F5.A_LE...][ix] = (goveq[F511.A_LE...]+goveq[F52.A_LE...])[ix]

goveq[F51M...., onlyna=TRUE] = goveq[F5....] - goveq[F511....] - goveq[F52....]
goveq[F51M...., onlyna=TRUE][which(goveq[F5....]==0)] = 0

names(dimnames(goveq))[1]='INSTR'
gov_equity=goveq[".A_LE...1998q4:"]

gov_equity[F51M..S124.,onlyna=TRUE]<-0
gov_equity[F5..S124.,onlyna=TRUE]<-gov_equity[F511..S124.]+gov_equity[F51M..S124.]+gov_equity[F52..S124.]

##### assumption: german gov't no holdings of F51M abroad. extremely simplifying but needed because no data
gov_equity[F51M.DE.S2.,onlyna=TRUE]<-0
saveRDS(gov_equity, file=file.path(data_dir, 'gov_equity.rds'))
