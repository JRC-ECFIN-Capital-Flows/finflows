library(MDstats); library(MD3)
`%&%` = function (..., collapse = NULL, recycle0 = FALSE)  .Internal(paste0(list(...), collapse, recycle0))

# Set data directory
if (!exists("data_dir")) data_dir = getwd()

##############################################################
## Shared helper: combine a nested list of mds() results   ##
## loadedbitslist is a list of lists (one per country),    ##
## each inner list containing one result per sector.       ##
## Fixed vs original: (1) initialisation now skips         ##
## try-error objects; (2) first element is not merged      ##
## into itself a second time.                              ##
##############################################################

combineloadedbits = function(loadedbitslist) {
  # Flatten nested list (country x sector) into a single list
  flat = unlist(loadedbitslist, recursive = FALSE)
  
  # Keep only successful mds() results
  ok = Filter(function(x) !is(x, 'try-error'), flat)
  
  n_total  = length(flat)
  n_failed = n_total - length(ok)
  if (n_failed > 0) message('combineloadedbits: ', n_failed, '/', n_total, ' calls failed and were skipped')
  if (length(ok) == 0L) stop('combineloadedbits: all ', n_total, ' mds() calls failed — nothing to combine')
  
  # Initialise with the first valid result, then merge the rest one by one
  resmd3 = ok[[1]]
  if (length(ok) > 1L) {
    for (i in seq(2L, length(ok))) {
      message(i, '/', length(ok))
      resmd3 = merge(resmd3, ok[[i]])
    }
  }
  return(resmd3)
}


##############################################################
#### EU and EA balance of payments exposure (aggregates)  ####
#### Source: Eurostat bop_eu6_q                           ####
#### No currency dimension — values always in MIO_EUR     ####
##############################################################

#NO it did not work. Let us load EVERYTHING
# it takes time but it's a way to avoid loading issues
bop_iip= mds('ESTAT/bop_eu6_q/........')

gc()
# and now filter
iamd3eu=bop_iip["..S1.S1+S121+S122+S123+S12K+S12M+S12T+S13+S1P+S1V+S1N+S11+S124+S12Q+S1M.ASS+LIAB.NSA..1999q1+1999q2+1999q3+1999q4+2000q1+2000q2+2000q3+2000q4+2001q1+2001q2+2001q3+2001q4+2002q1+2002q2+2002q3+2002q4+2003q1+2003q2+2003q3+2003q4+2004q1+2004q2+2004q3+2004q4+2005q1+2005q2+2005q3+2005q4+2006q1+2006q2+2006q3+2006q4+2007q1+2007q2+2007q3+2007q4+2008q1+2008q2+2008q3+2008q4+2009q1+2009q2+2009q3+2009q4+2010q1+2010q2+2010q3+2010q4+2011q1+2011q2+2011q3+2011q4+2012q1+2012q2+2012q3+2012q4+2013q1+2013q2+2013q3+2013q4+2014q1+2014q2+2014q3+2014q4+2015q1+2015q2+2015q3+2015q4+2016q1+2016q2+2016q3+2016q4+2017q1+2017q2+2017q3+2017q4+2018q1+2018q2+2018q3+2018q4+2019q1+2019q2+2019q3+2019q4+2020q1+2020q2+2020q3+2020q4+2021q1+2021q2+2021q3+2021q4+2022q1+2022q2+2022q3+2022q4+2023q1+2023q2+2023q3+2023q4+2024q1+2024q2+2024q3+2024q4+2025q1+2025q2+2025q3"]

saveRDS(iamd3eu, file.path(data_dir, 'bop_cps_euea.rds'))

gc()

##############################################################
#### Balance of payments + IIP of the EU institutions     ####
#### quarterly data                                       ####
##############################################################

#euibopraw=MDstats:::.mdEstat('Estat/bop_euins6_q'); gc()
euibopraw=mds('Estat/bop_euins6_q'); gc()
dim(euibopraw)
saveRDS(euibopraw, file.path(data_dir, 'bop_eui.rds'))

euiiipraw=MDstats:::.mdEstat('Estat/bop_euins6_iip'); gc()
dim(euiiipraw)
saveRDS(euiiipraw, file.path(data_dir, 'iip_eui.rds'))

##############################################################
#### Bilateral BOP flows — individual countries           ####
#### Source: Eurostat bop_c6_q                            ####
#### MIO_EUR hardcoded in query string                    ####
##############################################################

countries=c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
sectors=c("S1","S121","S122","S123","S12T","S13","S1P","S1V")

allsecload=function(countries,cpsto='S1.ASS',dataflow='bop_c6_q') {
  ll=paste0("Estat/",dataflow,"/Q.MIO_EUR..",sectors,".",cpsto,"..",countries)
  message(Sys.timo() , ': loading ',countries,'...')
  lapply(as.list(ll), \(x) try(mds(x,drop=FALSE),silent=TRUE))
}

aout = lapply(as.list(countries),allsecload,cpsto='S1.ASS')
aia=combineloadedbits(aout)
aia2=drop(aia)

save.image('C:/USers/Public/finflowsbuffer/bopiiploadedstuff.rda')

lout = lapply(as.list(countries), function(x) allsecload(x,cpsto='S1.LIAB'))
lia=combineloadedbits(lout)
lia2=drop(lia)
save.image('C:/USers/Public/finflowsbuffer/bopiiploadedstuff.rda')

iamd3=merge(aia2,lia2,along='STO',newcodes=c('ASS','LIAB'))

saveRDS(iamd3, file.path(data_dir, 'bop_cps.rds'))

gc()

##############################################################
#### Annual bilateral IIP stocks                          ####
#### Source: Eurostat bop_iip6_q (annual frequency)      ####
#### MIO_EUR hardcoded in query string                    ####
##############################################################

countries=c("EA19","EA20","EU27_2020","EU28","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
sectors=c("S1","S11","S121","S12T","S12K", "S13", "S124", "S12O", "S12Q", "S12M", "S1M")

allsecload=function(countries,cpsto='S1.A_LE',dataflow='bop_iip6_q') {
  ll=paste0("Estat/",dataflow,"/A.MIO_EUR..",sectors,".",cpsto,"..",countries)
  message(Sys.timo() , ': loading ',countries,'...')
  lapply(as.list(ll), \(x) try(mds(x,drop=FALSE),silent=TRUE))
}

aout = lapply(as.list(countries),allsecload,cpsto='S1.A_LE')
aaia=combineloadedbits(aout)
aaia2=drop(aaia)

save.image('C:/USers/Public/finflowsbuffer/bopiiploadedstuff.rda')

lout = lapply(as.list(countries), function(x) allsecload(x,cpsto='S1.L_LE'))
llia=combineloadedbits(lout)
llia2=drop(llia)
save.image('C:/USers/Public/finflowsbuffer/bopiiploadedstuff.rda')

iamd3=merge(aaia2,llia2,along='STO',newcodes=c('A_LE','L_LE'))

saveRDS(iamd3, file.path(data_dir, 'iip_cps_annual.rds'))

##############################################################
#### Quarterly bilateral IIP stocks                       ####
#### Source: Eurostat bop_iip6_q (quarterly frequency)   ####
#### MIO_EUR hardcoded in query string                    ####
##############################################################

countries=c("EA19","EA20","EU27_2020","EU28","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
sectors=c("S1","S11","S121","S12T","S12K", "S13", "S124", "S12O", "S12Q", "S12M", "S1M")

allsecload=function(countries,cpsto='S1.A_LE',dataflow='bop_iip6_q') {
  ll=paste0("Estat/",dataflow,"/Q.MIO_EUR..",sectors,".",cpsto,"..",countries)
  message(Sys.timo() , ': loading ',countries,'...')
  lapply(as.list(ll), \(x) try(mds(x,drop=FALSE),silent=TRUE))
}

aout = lapply(as.list(countries),allsecload,cpsto='S1.A_LE')
aia=combineloadedbits(aout)
aia2=drop(aia)

save.image('C:/USers/Public/finflowsbuffer/bopiiploadedstuff.rda')

lout = lapply(as.list(countries), function(x) allsecload(x,cpsto='S1.L_LE'))
lia=combineloadedbits(lout)
lia2=drop(lia)
save.image('C:/USers/Public/finflowsbuffer/bopiiploadedstuff.rda')

iamd3=merge(aia2,lia2,along='STO',newcodes=c('A_LE','L_LE'))

saveRDS(iamd3, file.path(data_dir, 'iip_cps.rds'))
