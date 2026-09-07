# Load required packages
library(MDstats)
library(MD3)


# Set the project directories
if (!exists("data_dir")) data_dir = getwd()


# Define paste0 operator
`%&%` = function (..., collapse = NULL, recycle0 = FALSE)  .Internal(paste0(list(...), collapse, recycle0))



# Country codes for IMF data
ccc='AUT+BEL+BGR+CYP+CZE+DEU+DNK+EST+ESP+FIN+FRA+GBR+GRC+HRV+HUN+IRL+ITA+LTU+LUX+LVA+MLT+NLD+POL+PRT+ROU+SWE+SVN+SVK+G163'
#G163 is euro area!

# Load IMF data
af5 = mds('IMF/IIP/' %&% ccc %&% '.A_P.O_F519_MV+P_F5_MV+P_F51_MV+P_F52_MV+P_F52_S123_MV+P_F5_S121_MV+P_F5_S122_MV+P_F5_S12R_MV+P_F5_S13_MV+P_F5_S1V_MV+P_F5_S1Z_MV.USD.Q',startPeriod = "1999q4")
lf5 = mds('IMF/IIP/' %&% ccc %&% '.L_P.O_F519_MV+P_F5_MV+P_F51_MV+P_F52_MV+P_F52_S123_MV+P_F5_S121_MV+P_F5_S122_MV+P_F5_S12R_MV+P_F5_S13_MV+P_F5_S1V_MV+P_F5_S1Z_MV.USD.Q',startPeriod = "1999q4")

af5[G163.P_F5_MV.]
af5[CZE..2022q4]

#### Save the two data sources
saveRDS(af5, file.path(data_dir, 'af5.rds'))
saveRDS(lf5, file.path(data_dir, 'lf5.rds'))

#### EXCHANGE RATE!
usd_eur = mds('ECB/EXR/Q.USD.EUR.SP00.E')
#####conversion in euros
#####conversion in euros (millions)
af5_eur=af5/(usd_eur*1e6)
lf5_eur=lf5/(usd_eur*1e6)

saveRDS(af5_eur, file.path(data_dir, 'af5_eur.rds'))
saveRDS(lf5_eur, file.path(data_dir, 'lf5_eur.rds'))


