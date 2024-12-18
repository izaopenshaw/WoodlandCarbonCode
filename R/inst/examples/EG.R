# Install package
devtools::install_github("izaopenshaw/WoodlandCarbonCode", force = TRUE) # force = TRUE to install updates.

# Load package
library(WoodlandCarbonCode)
source('R/functions.R')

# Example tree metrics
dbh    <- 74 # cm
height <- 24 # m
vol <- 50    # m^3

#==========Tariff Number===========
#==Method A: felled tree
tariff_vol_area(vol, dbh)

#==Method B: broadleaf
# Lookup species code in
data(sp_lookupdf)
rec <- sp_lookupdf[sp_lookupdf$General.for.genus=="Quercus",] # either by general genus
rec <- sp_lookupdf[sp_lookupdf$common_name=="oak",] # or by common name

spcode <- rec$single
btariff <-  broadleaf_tariff(spcode, height, dbh, sigma_dbh = 5, sigma_height = 1)
btariff
mercvol <- merchtreevol(dbh, tariff = btariff$tariff, sigma_dbh = 1, sigma_tariff = btariff$error)
mercvol
stemvolume <-  treevol(mtreevol = mercvol$volume, dbh, sigma_mtreevol = mercvol$error)
stemvolume

#==Stem Biomass
stembiomass <-  woodbiomass(stemvolume$stemvolume, rec$NSG, sigma_treevol = stemvolume$error)
stembiomass

#==Crown Biomass
crownbio <-  crownbiomass(rec$Crown, dbh, sigma_dbh = 1)
crownbio

#==Total above ground carbon
biomass <- (stembiomass$woodbiomass + crownbio$biomass)*0.5
biomass <- fc_agc(spcode, dbh, height, method="IPCC1", biome="temperate", "AGC")
biomass


# Import a dataframe add to methodology.R
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology")
df <- read.csv("df_7_24.csv")

# Change Classification to either 'broadleaf' or 'conifer'
table(df$Classification)
df$Classification[df$Classification == "coniferous"] <- "conifer"
spcodes <- lookspcode(df$Name, name_type="botanical", df$Classification, returnv="single")
df$spcode <- spcodes$spcodes
df$match_type <- spcodes$match_types

# Run fc_agc to get AGC from WCC
AGC <- fc_agc(df$spcode, df$DBH, df$Height, df$Classification, method = "Matthews1", biome = 'Temperate',returnv = "all")
df <- cbind(df, AGC[,4:10])
colnames(df[48]) <- "AGC_WCC1"

# Example dataframe
df <- data.frame()

spcodes <- lookspcode(df$Name, name_type="botanical", df$Classification, returnv="single")
df$spcode <- spcodes$spcodes
df$match_type <- spcodes$match_types

# Run fc_agc to get AGC from WCC
AGC <- fc_agc(df$spcode, df$DBH, df$Height, df$Classification, method = "Matthews1", biome = 'Temperate',returnv = "all")
df <- cbind(df, AGC[,4:10])

# TLS
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology")
conifer <- read.csv("OG_Data/QSM_conifer.csv")
df <- conifer[,c(11,12,22:24,36:38,44,46)]
df$Name <- paste(df$genus, df$species, sep = " ")
df$DBH <- (df$DBHcyl+df$DBHqsm)/2

df$Classification <- 'conifer'
spcodes <- lookspcode(df$Name, name_type="botanical", df$Classification, returnv="single")
df$spcode <- spcodes$spcodes
df$match_type <- spcodes$match_types




