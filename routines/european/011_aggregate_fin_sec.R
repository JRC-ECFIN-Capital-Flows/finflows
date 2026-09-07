library(MDstats); 

# Set data directory
#data_dir= file.path(getwd(),'data')
if (!exists("data_dir")) data_dir = '\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/data/filled'
source('\\\\s-jrciprnacl01p-cifs-ipsc.jrc.it/ECOFIN/FinFlows/githubrepo/finflows/routines/utilities.R')
gc()


## load filled iip bop
aa=readRDS(file.path(data_dir,'aa_iip_dip.rds')); gc()
ll=readRDS(file.path(data_dir,'ll_iip_dip.rds')); gc()
gc()


##--------------------------------------------------------------------
## Tx7 Aggregate (sum over all functional categories -again- except F7)
##--------------------------------------------------------------------
## -Assets-
aa[....._Tx7..]=NA;gc()
daa=as.data.table(aa,.simple=TRUE); gc()
if (anyNA(daa[,COUNTERPART_AREA])) daa=daa[!is.na(COUNTERPART_AREA),]
dacast=dcast(daa, ... ~ FUNCTIONAL_CAT, value.var = 'obs_value'); gc()
dacast=dacast[!(is.na(`_D`) & is.na(`_P`) & is.na(`_O`)& is.na(`_R`)),]
dacast[,Tx7:=apply(.SD[,c('_D','_P','_O','_R')],1,sum,na.rm=TRUE)]


#dacast[['Tx7']]=apply(dacast[,c('_D','_P','_O','_R')],1,sum,na.rm=TRUE);gc()
dacast2=copy(dacast[,intersect(c(colnames(daa),'Tx7'),colnames(dacast)),with=FALSE]);gc()
colnames(dacast2)[colnames(dacast2)=='Tx7']='obs_value';gc()
dacast2[,FUNCTIONAL_CAT:='_Tx7'];gc()

#rm(dacast); rm(aa); gc()
daa=rbind(daa,dacast2,fill=TRUE);gc()     
aa2=as.md3(daa);gc()

aa=copy(aperm(aa2, c(1:6,8,7)))


dim(aa)
gc()

aa[.AT.S1.S1.LE..2022q4.WRL_REST]

## -Liabilities-
ll[....._Tx7..]=NA;gc()
dll=as.data.table(ll,.simple=TRUE); gc()
if (anyNA(dll[,COUNTERPART_AREA])) dll=dll[!is.na(COUNTERPART_AREA),]
dacast=dcast(dll, ... ~ FUNCTIONAL_CAT, value.var = 'obs_value'); gc()  # removed id.vars=1:8
dacast[,`_Tx7`:=apply(.SD[,c('_D','_P','_O')],1,sum,na.rm=TRUE)];gc()
dacast2=copy(dacast[,intersect(c(colnames(dll),'_Tx7'),colnames(dacast)),with=FALSE]);gc()
colnames(dacast2)[colnames(dacast2)=='_Tx7']='obs_value';gc()  # by name, not NCOL position
dacast2[,FUNCTIONAL_CAT:='_Tx7'];gc()

dll=rbind(dll,dacast2,fill=TRUE);gc()     
ll2=as.md3(dll);gc()

ll=aperm(copy(ll2), c(1:6,8,7))

gc()

ll[.WRL_REST.S1.S1.LE..2022q4.AT]




##--------------------------------------------------------------------
## Fx7 Aggregate (sum over all financial instruments except F7, F21)
##--------------------------------------------------------------------

## -Assets-
aa[Fx7.......]=NA
daa=as.data.table(aa,.simple=TRUE); gc()
if (anyNA(daa[,COUNTERPART_AREA])) daa=daa[!is.na(COUNTERPART_AREA),]

fx7_instrs <- c('F2M','F3','F4','F511','F51M','F52','F6','F81','F89')
by_dims <- setdiff(names(daa), c('INSTR','obs_value'))
dacast2 <- daa[INSTR %in% fx7_instrs,
               .(obs_value=sum(obs_value, na.rm=TRUE), INSTR='Fx7'),
               by=by_dims]; gc()

daa=rbind(daa, dacast2, fill=TRUE); gc()
aa2=as.md3(daa); gc()
aa=aperm(copy(aa2), c(1:6,8,7))
rm(daa,dacast2,aa2); gc()


dim(aa)
aa[.AT.S1.S1.LE._Tx7.2022q4.EA20+WRL_REST]

## -Liabilities-
ll[Fx7.......]=NA
dll=as.data.table(ll,.simple=TRUE); gc()
if (anyNA(dll[,COUNTERPART_AREA])) dll=dll[!is.na(COUNTERPART_AREA),]
if (anyNA(dll[,REF_AREA])) dll=dll[!is.na(REF_AREA),]

fx7_instrs <- c('F2M','F3','F4','F511','F51M','F52','F6','F81','F89')
by_dims <- setdiff(names(dll), c('INSTR','obs_value'))
dacast2 <- dll[INSTR %in% fx7_instrs,
               .(obs_value=sum(obs_value, na.rm=TRUE), INSTR='Fx7'),
               by=by_dims]; gc()

dll=rbind(dll, dacast2, fill=TRUE); gc()
ll2=as.md3(dll); gc()
ll=aperm(copy(ll2), c(1:6,8,7))
rm(dll,dacast2,ll2); gc()

dim(ll)
ll[.WRL_REST.S1.S1M+S11.LE._Tx7.2022q4.AT]




saveRDS(aa,file.path(data_dir,'aa_iip_agg.rds'))
saveRDS(ll,file.path(data_dir,'ll_iip_agg.rds'))

saveRDS(aa,file.path(data_dir,'vintages/aa_iip_agg_' %&% format(Sys.time(),'%F') %&% '_.rds'))
saveRDS(ll,file.path(data_dir,'vintages/ll_iip_agg_' %&% format(Sys.time(),'%F') %&% '_.rds'))

