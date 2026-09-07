# Load required packages
library(MDstats)
library(MD3)

# Set data directory
if (!exists("data_dir")) data_dir = getwd()

# Load other equity assets/liabilities from ECB QSA
otheq_al=mds('ECB/QSA/Q.N..W0+W1+W2...N.A+L.LE.F512+F519._Z._Z.XDC..S.V.N._T')
saveRDS(otheq_al, file=file.path(data_dir, 'otheq_al.rds'))

# Load existing processed data
otheq_al <- readRDS(file.path(data_dir, 'otheq_al.rds'))
names(dimnames(otheq_al)) =gsub("^INSTR.*$","INSTR",names(dimnames(otheq_al)))
names(dimnames(otheq_al)) =gsub("^COUNTER.*$","COUNTERPART_SECTOR",names(dimnames(otheq_al)))
otheq_al=aperm(otheq_al,c("INSTR","REF_AREA","REF_SECTOR","COUNTERPART_SECTOR","ACCOUNTING_ENTRY","TIME"))

otheq=otheq_al[....A.]

dimnames(otheq)$COUNTERPART_SECTOR = c(W0='S0' , W1='S2')[dimnames(otheq)$COUNTERPART_SECTOR]

otheq[..S2.S0.] =otheq_al[..S1.W1.L.]
otheq[..S2.S1.] =otheq_al[..S1.W1.L.]
otheq[..S1.S1.] = otheq[..S1.S0.] -otheq[..S1.S2.] 
otheq[..S0.S1.] = otheq[..S1.S1.] +otheq[..S2.S1.] 

# Load main data
aall <- readRDS(file.path(data_dir, 'intermediate_domestic_data_files/aall_cp.rds'))
af51m=aall['F51M....LE._T+FND.1999q1:']

otheq=add.dim(otheq,.dimname = 'CUST_BREAKDOWN',.dimcodes = '_T')
dimnames(otheq)[[1]]='_T'

# Load exchange rates
xrates <- readRDS(file.path(data_dir, 'xrates_final.rds'))

# Apply exchange rates
otherequity = otheq*xrates[LE..]
names(dimnames(otherequity)) =gsub("^INSTR.*$","INSTR_ASSET",names(dimnames(otherequity)))

af51=merge(otherequity, af51m,along = 'INSTR_ASSET')
dimnames(af51)$INSTR_ASSET[3] = 'F51M'

af51[".F51M....",onlyna=TRUE] <- af51[".F512...."] + af51[".F519...."]

af51=aperm(af51,c("CUST_BREAKDOWN","REF_AREA","COUNTERPART_SECTOR","REF_SECTOR","INSTR_ASSET","TIME"))
af51["..S1...",onlyna=TRUE] <- af51["..S0..."]-af51["..S2..."]

# Load BOP data for F51M
bopf51mq=mds('Estat/bop_iip6_q/Q.MIO_EUR.FA__D__F519+FA__O__F519+FA__D__F512+FA__P__F512+FA__D__F51M+FA__D__F52+FA__P__F52..S1.A_LE+L_LE.WRL_REST.')
saveRDS(bopf51mq, file=file.path(data_dir, 'bopf51mq.rds'))

bopf51m_a=mds('Estat/bop_iip6_q/A.MIO_EUR.FA__D__F519+FA__O__F519+FA__D__F512+FA__P__F512+FA__D__F51M+FA__D__F52+FA__P__F52..S1.A_LE+L_LE.WRL_REST.')

bopf51ma=copy(bopf51m_a); frequency(bopf51ma)='Q'
saveRDS(bopf51ma, file=file.path(data_dir, 'bopf51ma.rds'))
### this created issues!!!
af51[FND..S2..F51M.]<-af51[FND..S0..F51M.]

names(dimnames(bopf51ma))[1]=names(dimnames(bopf51mq))[1]='REF_AREA'
names(dimnames(bopf51mq))[2]='REF_SECTOR'
names(dimnames(bopf51ma))[2]='REF_SECTOR'

af51['_F+_O+_P+_R.....'] <- NA

####QUARTERLY
af51[FND..S2..F519.,onlyna=TRUE]<-bopf51mq["..A_LE.FA__D__F519.1998q4:"]
af51[FND..S2..F512.,onlyna=TRUE]<-bopf51mq["..A_LE.FA__D__F512.1998q4:"]
af51[FND..S2..F51M.,onlyna=TRUE]<-bopf51mq["..A_LE.FA__D__F51M.1998q4:"]
af51[FND..S2..F52. ,onlyna=TRUE]<-bopf51mq["..A_LE.FA__D__F52.1998q4:" ]

af51["_O..S2..F519.",onlyna=TRUE]<-bopf51mq["..A_LE.FA__O__F519.1998q4:"]
af51["_P..S2..F512.",onlyna=TRUE]<-bopf51mq["..A_LE.FA__P__F512.1998q4:"]
af51["_P..S2..F52.",onlyna=TRUE]<-bopf51mq["..A_LE.FA__P__F52.1998q4:"]

# Liabilities
bopf51mq_liab=copy(bopf51mq)
names(dimnames(bopf51mq_liab))[2]='COUNTERPART_SECTOR'

af51[FND...S2.F512.,onlyna=TRUE]<-bopf51mq_liab["..L_LE.FA__D__F512.1998q4:"]
af51[FND...S2.F519.,onlyna=TRUE]<-bopf51mq_liab["..L_LE.FA__D__F519.1998q4:"]
af51[FND...S2.F51M.,onlyna=TRUE]<-bopf51mq_liab["..L_LE.FA__D__F51M.1998q4:"]
af51[FND...S2.F52., onlyna=TRUE]<-bopf51mq_liab["..L_LE.FA__D__F52.1998q4:" ]

af51["_O...S2.F519.",onlyna=TRUE]<-bopf51mq_liab["..L_LE.FA__O__F519.1998q4:"]
af51["_P...S2.F512.",onlyna=TRUE]<-bopf51mq_liab["..L_LE.FA__P__F512.1998q4:"]
af51["_P...S2.F52.",onlyna=TRUE]<-bopf51mq_liab["..L_LE.FA__P__F52.1998q4:"]

########### ANNUAL
af51[FND..S2..F512.,onlyna=TRUE]<-bopf51ma["..A_LE.FA__D__F512.1998q4:"]
af51[FND..S2..F519.,onlyna=TRUE]<-bopf51ma["..A_LE.FA__D__F519.1998q4:"]
af51[FND..S2..F51M.,onlyna=TRUE]<-bopf51ma["..A_LE.FA__D__F51M.1998q4:"]
af51[FND..S2..F52., onlyna=TRUE]<-bopf51ma["..A_LE.FA__D__F52.1998q4:" ]

af51["_O..S2..F519.",onlyna=TRUE]<-bopf51ma["..A_LE.FA__O__F519.1998q4:"]
af51["_P..S2..F512.",onlyna=TRUE]<-bopf51ma["..A_LE.FA__P__F512.1998q4:"]
af51["_P..S2..F52.",onlyna=TRUE]<-bopf51ma["..A_LE.FA__P__F52.1998q4:"]

bopf51ma_liab=copy(bopf51ma)
names(dimnames(bopf51ma_liab))[2]='COUNTERPART_SECTOR'

af51[FND...S2.F512.,onlyna=TRUE]<-bopf51ma_liab["..L_LE.FA__D__F512.1998q4:"]
af51[FND...S2.F519.,onlyna=TRUE]<-bopf51ma_liab["..L_LE.FA__D__F519.1998q4:"]
af51[FND...S2.F51M.,onlyna=TRUE]<-bopf51ma_liab["..L_LE.FA__D__F51M.1998q4:"]
af51[FND...S2.F52., onlyna=TRUE]<-bopf51ma_liab["..L_LE.FA__D__F52.1998q4:" ]

af51["_O...S2.F519.",onlyna=TRUE]<-bopf51ma_liab["..L_LE.FA__O__F519.1998q4:"]
af51["_P...S2.F512.",onlyna=TRUE]<-bopf51ma_liab["..L_LE.FA__P__F512.1998q4:"]
af51["_P...S2.F52.",onlyna=TRUE]<-bopf51ma_liab["..L_LE.FA__P__F52.1998q4:"]

saveRDS(af51, file=file.path(data_dir, 'af51m_INT.rds'))

#af51[FND..S2..F51M.]<-af51[FND..S0..F51M.]
### ATTENTION here some S2 are bigger than S0... so we impose the max between the difference and 0
# Calculate the differences first
af51["_T+_F+_O+_P+_R...S1..", onlyna=TRUE] <- af51["_T+_F+_O+_P+_R...S0.."] - af51["_T+_F+_O+_P+_R...S2.."]
af51["_T+_F+_O+_P+_R..S1...", onlyna=TRUE] <- af51["_T+_F+_O+_P+_R..S0..."] - af51["_T+_F+_O+_P+_R..S2..."]
# Then set negative values to zero
af51["...S1.."][which(af51["...S1.."] < 0)] <- 0
af51["..S1..."][which(af51["..S1..."] < 0)] <- 0

af51[....F51M.,onlyna=TRUE]<-af51[....F512.]+af51[....F519.]

saveRDS(af51, file=file.path(data_dir, 'af51_intermediate.rds'))

#### CREATE DIMENSION FOR PARTIAL SUMS
af51["_S....."]=NA
gc()

### unflagging necessary to get the "obs_status" away
af51=unflag(af51)
af51_S=as.data.table(af51,.simple=TRUE)
gc()

af51cast=dcast(af51_S, ... ~ CUST_BREAKDOWN, id.vars=1:7, value.var = 'obs_value')
gc()

# Use _T when available, otherwise calculate sum
af51cast[['_S']] <- ifelse(
  !is.na(af51cast[['_T']]), 
  af51cast[['_T']],  # Use existing _T when available
  rowSums(af51cast[,c('FND','_P','_O')], na.rm=TRUE)  # Calculate from components when _T is missing
)
gc()

af51cast2=copy(af51cast[,intersect(c(colnames(af51_S),'_S'),colnames(af51cast)),with=FALSE])
gc()
colnames(af51cast2)[NCOL(af51cast2)]='obs_value'
gc()
af51cast2[,CUST_BREAKDOWN:='_S']
gc()
af51_S=rbind(af51_S,af51cast2,fill=TRUE)
gc()
af51_M=as.md3(af51_S)
gc()
af51=copy(af51_M)

names(dimnames(af51))[5]='INSTR'

af51["_T.....",onlyna=TRUE]<-af51["_S....."]
af51=af51["_T+FND+_O+_P....."]
af51[".CZ.S2+S0.S1.F51M.2022q4"]
saveRDS(af51, file=file.path(data_dir, 'af51.rds'))
