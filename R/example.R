devtools::install_github("izaopenshaw/WoodlandCarbonCode")
devtools::install_github("izaopenshaw/WoodlandCarbonCode", force = TRUE)

library(WoodlandCarbonCode)

# --------- Get Tariff Number ---------
# Example tree metrics
dbh    <- 74 # cm
height <- 24 # m
vol <- 50    # m^3

# Method A: felled tree
fc_tariff_vol_area(vol, dbh)

# Method B: broadleaf
# Lookup species code in data(sp_lookupdf)
rec <- sp_lookupdf[sp_lookupdf$General.for.genus=="Quercus",] # either by general genus
rec <- sp_lookupdf[sp_lookupdf$common_name=="oak",] # or by common name
spcode <- rec$single
tariff <- fc_broad_tariff(spcode, height, dbh)
tariff

# Method C: conifer
rec_c <- sp_lookupdf[sp_lookupdf$General.for.classification=="coniferous",]
spcode_c <- rec_c$single
fc_con_tariff(spcode_c, height, dbh)

# Method D: dense and dark stands
fc_stand_tariff(spcode, height)

# --------- Get tree volumes ---------
# Merchantable tree volume
mercvol <- fc_merchtreevol(tariff, dbh)

# Stem volume
stemvol <- fc_treevol(mercvol, dbh)

# Stem Biomass
stembiomass <- fc_woodbiomass(stemvol, rec$NSG)

# Crown Biomass
crownbiomass <- fc_crownbiomass(rec$Crown, dbh)

# Total above ground carbon
(stembiomass + crownbiomass) * 0.5
fc_agc(spcode, dbh, height, "AGC")

# Root Biomass
rootbiomass <- fc_rootbiomass(rec$Root, dbh)


fc_broad_sap_seedling2C(100)
fc_con_sap_seedling2C(100)


#multiple
#note should work with vectors, but not at the moment
sp <- c('OK',"AH","SP","SS")
d <- c(70,35,50,60)
h <- c(17,20,25,28)
df <- data.frame(sp, d, h)
#empty field to put results
df$carbon_ton <- NaN

fc_agc(spcode,dbh,height,returnv="AGC")

for (i in 1:nrow(df)){
  r <- df[i,]
  df[i,]$carbon_ton <- fc_agc(spcode=r$sp,dbh=r$d,height=r$h,returnv="AGC")
}

df <- read.csv("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology/Stem_map_Pearcelands&Conifer.csv")
df$code <- NA
# * to check spelling first with biomass package

for(i in 1:nrow(df)){
  rec <- sp_lookupdf[sp_lookupdf$latin_name == df$Name[i],]

  if(nrow(rec) == 0){
    rec <- sp_lookupdf[sp_lookupdf$General.for.genus == df$Genus[i],]
    if(nrow(rec) == 0){
      rec <- sp_lookupdf[sp_lookupdf$General.for.classification == df$Classification[i],]
    }
  }
  df$code[i] <- rec$short
}

#itree result
#https://mytree.itreetools.org#/benefits/error=&warning=&creationDate=1652891933748&group=&note=/uuid=c4a556e9-8e19-4846-98e7-569bb8ad15d2&formatted=Richmond+TW9+3AB%2C+UK&entered=Royal+Botanic+Gardens%2C+Richmond+TW9+3AB%2C+UK&city=Washington&county=District+of+Colombia&latitude=51.47884371687695&longitude=-0.2927330131994329&nation=US&stateAbbr=DC&zip=20003&pin=false/uuid=f5b7aebc-00cf-4e29-8c7d-c62e88faec18&common=Northern+red+oak&condition=0&diameter=27.559055118110237&exposure=0&measurementType=true&namingType=true&scientific=Quercus+rubra&species=QURU&type=&trillionTrees=null/uuid=65e83431-d13a-4b5f-bcae-585ccd59e158&direction=-1&distance=-1&proximity=2&vintage=-1/uuid=d3cee964-3d49-4249-b427-d60c9dfd59fa&optIn=false&emailed=true"

###################
# progression of error
# example for volume
# assuming 5% error for diameter and 2 m height
v <- pro_error_vol(70,2.8,17,2)

d<-60:80
dsd <- h*0.05
h <- 7:27
hsd <- h*0.1

p <- pro_error_vol(d,dsd,h,hsd)

#examples using oak with 20% vol sd and 4 m3

tc <- pro_error_carbon(4,0.8,0.5747142,0.0708236,0.488,0.003)


##########################need to do species look up which is fun
library(BIOMASS)
correctTaxo(genus = "Quercus", species = "robur")
correctTaxo(genus = "Quercus robur")
wd <- getWoodDensity(
  genus = "Quercus",
  species= "robur"
)

AGB <- computeAGB(d, wd$meanWD, h)



# For testing
spcode=r$code
dbh=r$DBH
height=r$Height

tariff <- fc_broad_tariff(tarifflokupcode,h,d)
mercvol <- fc_merchtreevol(tariff,dbh)
stemvol1 <- fc_treevol(mercvol,dbh)
stembiomass <- fc_woodbiomass(stemvol1,rec$NSG)
crownbiomass <- fc_crownbiomass(rec$Crown,dbh)
AGC <- (stembiomass + crownbiomass) * 0.5

tariff
mercvol
stemvol1
stembiomass
crownbiomass
AGC

nominal_specific_gravity <- NSG
usethis::use_data(nominal_specific_gravity, overwrite = TRUE)
