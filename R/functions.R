##########################################################
#tree carbon values
##########################################################
#' @title tariff number from vol and area
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param spcode species code
#' @param dbh in cm
#' @param height in m
#' @param returnv what to return, Either AGC or All
#' @returns either AGC in tonnes [default]
#' or All list of tariff number, merchantable volume m3, stem volume m3, stem biomass (tonnes), stem carbon (tonnes), canopy carbon (tonnes) and root carbon (tonnes)
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).
#'

fc_agc <- function(spcode,dbh,height,returnv="AGC"){

  if(!is.numeric(dbh))stop("dbh must be numeric")
  if(!is.numeric(height))stop("height must be numeric")

  rec <- sp_lookupdf[sp_lookupdf$short == spcode,]
  tarifflokupcode <- rec$single
  type <- rec$type

  if (returnv == "AGC"){
    if (type == 'broadleaf'){
      tariff <- fc_broad_tariff(tarifflokupcode,height,dbh)
      mercvol <- fc_merchtreevol(tariff,dbh)
      stemvol1 <- fc_treevol(mercvol,dbh)
      stembiomass <- fc_woodbiomass(stemvol1,rec$NSG)
      crownbiomass <- fc_crownbiomass(rec$Crown,dbh)
      AGC <- (stembiomass + crownbiomass) * 0.5
    } else if (type == "conifer") {
      tariff <- fc_con_tariff(tarifflokupcode,height,dbh)
      mercvol <- fc_merchtreevol(tariff,dbh)
      stemvol1 <- fc_treevol(mercvol,dbh)
      stembiomass <- fc_woodbiomass(stemvol1,rec$NSG)
      crownbiomass <- fc_crownbiomass(rec$Crown,dbh)
      AGC <- (stembiomass + crownbiomass) * 0.5
    }
  } else {
    if (type == 'broadleaf'){
      tariff <- fc_broad_tariff(tarifflokupcode,height,dbh)
      mercvol <- fc_merchtreevol(tariff,dbh)
      stemvol1 <- fc_treevol(mercvol,dbh)
      stembiomass <- fc_woodbiomass(stemvol1,rec$NSG)
      crownbiomass <- fc_crownbiomass(rec$Crown,dbh)
      rootbiomass <- fc_rootbiomass(rec$Root,dbh)
      AGC <- (stembiomass + crownbiomass) * 0.5
      list(sp_code = spcode, tariff=tariff,merc_vol=mercvol,stem_vol=stemvol1,stem_biomass=stembiomass,crown_biomass=crownbiomass,root_biomass=rootbiomass,AGC=AGC)
    } else if (type == "conifer") {
      tariff <- fc_con_tariff(tarifflokupcode,height,dbh)
      mercvol <- fc_merchtreevol(tariff,dbh)
      stemvol1 <- fc_treevol(mercvol,dbh)
      stembiomass <- fc_woodbiomass(stemvol1,rec$NSG)
      crownbiomass <- fc_crownbiomass(rec$Crown,dbh)
      rootbiomass <- fc_rootbiomass(rec$Root,dbh)
      AGC <- (stembiomass + crownbiomass) * 0.5
      list(sp_code = spcode,tariff=tariff,merc_vol=mercvol,stem_vol=stemvol1,stem_biomass=stembiomass,crown_biomass=crownbiomass,root_biomass=rootbiomass,AGC=AGC)
    }
  }
  return(AGC)
}

##########################################################
# Tariff number from vol and tree basal area
##########################################################
#' @title tariff number from vol and area
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param vol = tree volume in m^3
#' @param dbh in cm
#' @returns  tariff number
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).
#' (Equation 1)
#'
#'
#'
#'
fc_tariff_vol_area <- function(vol,dbh){
  ba <- (pi * dbh^2)/40000                      # tree basal area in m^2
  a1 <- (vol - 0.005002986)/(ba - 0.003848451)
  (3.174106384 * a1) + 0.138763302
}

##########################################################
#FC conifer  tree tariff number
##########################################################
#' @title Conifer tree tariff number
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param height tree height in metres
#' @param dbh diameter at breast height in cm
#' @returns  tariff number
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).
#'
#'
#'
#'
# needs to round down
fc_con_tariff <- function(spcode,height,dbh){
  #height in m
  #dbh in cm
  #lookup
  rec <- tariff_coniferdf[tariff_coniferdf$abbreviation == spcode,]
  floor(rec$a1 + (rec$a2 * height) + (rec$a3 * dbh))
}
##########################################################
#FC board leaf tree tariff number
##########################################################
#' @title C board leaf tree tariff number
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param height tree height in metres
#' @param dbh diameter at breast height in cm
#' @param spcode species code
#' @returns  tariff number
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).
#'
#'
fc_broad_tariff <- function (spcode,height,dbh){
  rec <- tariff_broaddf[tariff_broaddf$abbreviation == spcode,]
  floor(rec$a1 + (rec$a2 * height) + (rec$a3 * dbh) + (rec$a4 * dbh * height))
}

##########################################################
#FC tariff number by stand height
##########################################################
#' @title tariff number by stand height
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param height tree height in metres
#' @param spcode species code
#' @returns  tariff number
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).
#'
#'
fc_stand_tariff <- function(spcode,height){
  #height in m
  #dbh in cm
  #lookup
  tariff <- a1 + (a2 * h) + (a3 * h^2)
}


##########################################################
#FC tree merchantable volume
##########################################################
#' @title Forestry merchantable volume
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param tarrif tree number tariff
#' @param dbh diameter at breast height in cm
#' @returns  volume m^3
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).
#'
fc_merchtreevol <- function (tariff, dbh){
  ba <- (pi * dbh^2)/40000
  a2 <- 0.315049301 * (tariff - 0.138763302)
  a1 <- (0.0360541 * tariff) - (a2 * 0.118288)
  vol <- a1 + (a2 * ba)
  if(vol < 0){
    vol = 0
  }
  return(vol)
}
##########################################################
#FC tree volume
##########################################################
#' @title Forestry commission tree wood volume
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param mtreevol = mercantable tree volume
#' @returns  volume m3
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).

fc_treevol <- function(mtreevol,dbh){
  dbh <- round(dbh)
  #lookup
  if (dbh < maxstemvol & dbh > 6.5){
    cf <- stemvol[stemvol$dbh..cm. == dbh,]$X   # max dbh from stemvol lookup is 7
    return (cf * mtreevol)
  } else {
    return (mtreevol)
  }
}
##########################################################
#FC wood biomass
##########################################################
#' @title Forestry commission wood biomass
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param treevol tree volume m^3
#' @param nsg Nominal Specific Gravity (NSG)
#' @returns  biomass (oven dry tonnes)
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).

fc_woodbiomass <- function(treevol, nsg){
  treevol * nsg
}
##########################################################
#FC crownbiomass
##########################################################
#' @title Forestry commission root biomass estimates
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param dbh diameter at breast height in cm
#' @param spcode species code
#' @returns  biomass (oven dry tonnes)
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).

fc_crownbiomass <- function(spcode,dbh){
  #7 to 50 cm dbh
  #lookup
  rec <- crown_biomasdf[crown_biomasdf$Code == spcode,]
  if (dbh <= 50) {cb <- cbiomass <- rec$b1 * dbh^rec$p} else
  { cb <- rec$A + rec$b2 * dbh}
  return (cb)
}
##########################################################
#FC rootbiomass
##########################################################
#' @title Forestry commission root biomass estimates
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param dbh in cm
#' @param spcode species code
#' @returns biomass (oven dry tonnes)
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018).

fc_rootbiomass <- function(spcode,dbh){
  #lookup
  rec <- root_biomassdf[root_biomassdf$Code == spcode,]

  if (dbh <= 30) {rb <- rec$b1 * dbh^2.5} else
  {rb <- rec$a + rec$b2 * dbh}
  return (rb)
}
##########################################################
#Carbon to CO2e
##########################################################
#' @title Carbon to CO2 equivalent
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param carbon carbon
#' @returns CO2e C02 equivalent
#' @references


c2co2e <- function(carbon){
  carbon * 44/12
}

##########################################################
#Plant biomass conversion to carbon
##########################################################
#' @title Calculates biomass conversion to carbon
#' @description
#' Calculates biomass conversion to carbon using multiple methods/citations
#' Mathews: simpliest giving 0.5
#' IPCC simple: 0.477
#' IPCC by type and biome
#' Mathews simple: 0.483 with 95% confidence interval 0.003
#' Mathews by type and biome
#' @author Justin Moat. J.Moat@kew.org
#' @param biomass (usually kg or metric tonnes)
#' @param method
#' Thomas_sim = simple with biomass 0.483 and 95% ci of 0.003, can be used for error progression (default)
#' Matthews = simpliest with biomass:carbon 0.5 (Matthews 1993)
#' IPCC_sim = simple with biomass 0.477 (IPCC 2006)
#' IPCC = lookup by type (Angiosperm or Conifer) and biome (tropical, Subtropical/Mediterranean,Temperate/Boreal,all)
#' Thomas = lookup by type (Angiosperm or Conifer) and biome (tropical, Subtropical/Mediterranean,Temperate/Boreal,all)
#' @param ci confidence interval returns TRUE or False (default)
#' @param type Angiosperm or Conifer
#' @param  biome tropical, Subtropical/Mediterranean,Temperate/Boreal,all
#' @return either carbon value or list of carbon value with sd error
#' @references
#' Thomas, Sean C., and Adam R. Martin. "Carbon content of tree tissues: a synthesis." Forests 3.2 (2012): 332-352. https://www.mdpi.com/1999-4907/3/2/332
#' IPCC. Forest lands. Intergovernmental Panel on Climate Change Guidelines for National Greenhouse Gas Inventories; Institute for Global Environmental Strategies (IGES): Hayama,Japan, 2006; Volume 4, p. 83.
#' Matthews, G.A.R. (1993) The Carbon Content of Trees. Forestry Commission Technical Paper 4. Forestry Commission, Edinburgh. 21pp. ISBN: 0-85538-317-8


biomass2c <-  function(biomass,method,ci,type,biome){
  #or add more detail to this
  if (method == "Matthews"){biomass * 0.5}
  if (method == "IPCC_s"){biomass * 0.477}
  if (method == "Thomas 2012"){
    #lookup
  }
  if (method == "IPCC"){
    #lookup
  }
  else
  {biomass * 0.483}#Thomas 2012 default error = 0.003

}
##########################################################
#FC Conifer seedlings and saplings to carbon
##########################################################
#' @title Conifer seedlings and saplings to carbon
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param heightincm tree height in metres
#' @returns  carbon in tonnes
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018)
#'
#'
#
fc_con_sap_seedling2C <- function(heightincm){
  b <- tail(seedlings_conifer[seedlings_conifer$height.cm <= heightincm,],1)
  t <- head(seedlings_conifer[seedlings_conifer$height.cm >= heightincm,],1)
  rt <- (t$height.cm - heightincm)/(t$height.cm-b$height.cm)
  if ((nrow(b) == 0) |  (nrow (t) == 0)){
    NULL
  } else if (is.nan(rt)){
    t$Carbon.kg
  } else {
    b$Carbon.kg + ((t$Carbon.kg - b$Carbon.kg) * rt)
  }
}
##########################################################
#FC Broadleaf seedlings and saplings to carbon
##########################################################
#' @title Broadleaf seedlings and saplings to carbon
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param height = tree height in metres
#' @returns  carbon in tonnes
#' @note just uses simple linear relationship to get between measures
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018)
#'
#'
#
fc_broad_sap_seedling2C <- function(heightincm){
  #get first and last
  #note max is 1000
  #min is 1 cm
  b <- tail(seedlings_broad[seedlings_broad$height.cm <= heightincm,],1)
  t <- head(seedlings_broad[seedlings_broad$height.cm >= heightincm,],1)
  rt <- (t$height.cm - heightincm)/(t$height.cm-b$height.cm)
  if ((nrow(b) == 0) |  (nrow (t) == 0)){
    NULL
  } else if (is.nan(rt)){
    t$Carbon.kg
  } else {
    b$Carbon.kg + ((t$Carbon.kg - b$Carbon.kg) * rt)
  }
}



##########################################################
# Progression of errors from volume, density, biomassc = carbon
##########################################################
#' @title Carbon progression of errors
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param volsd volume sd
#' @param densd wood density sd
#' @param biomsd biomass sd
#' @param nruns number of iteration, suggest 10,000 as min and 100,000 is a good number
#' @param returnv if null then mean and sd is returned else vector of quantiles ie c(5,50,95)/100 will return 5%, mean and 95% quantiles.
#' @returns  either vector of mean and sd or vector of quantiles
#' @references
#'
#vol <- 100
#volsd <- 10
#den <- 0.5
#densd <- 0.005
#biom <- 0.5
#biomsd <- 0.0025
#nruns <-100000
#returnsv <- c(5,50,95)/100
# pro_error_carbon(vol,volsd,den,densd,biom,biomsd,nruns=100000,returnsv=c(5,50,95)/100)
# pro_error_carbon(vol,volsd,den,densd,biom,biomsd,nruns=100000)
pro_error_carbon <- function(vol,volsd,den,densd,biom,biomsd,nruns=10000, returnsv=NULL) {
  vol <- rnorm(nruns,mean=vol,sd=volsd)
  den <- rnorm(nruns,mean=den,sd=densd) # middle of the road
  biomass <- rnorm(nruns,mean=biom,sd=biomsd) # conifer or angiosperm
  carbt <- vol * den * biomass
  if (!is.null(returnsv)){
    quantile(carbt,probs=returnsv)
  } else {
    c(mean = mean(carbt),sd= sd(carbt))
  }
}


#AGB = 0.0673 * (WD * H * D^2)^0.976
##########################################################
#progression of errors from height and dbh to volume
##########################################################
#' @title Volume progression of errors
#' @description
#' @author Justin Moat. J.Moat@kew.org
#' @param dbh diameter at breast height (dbh) in cm
#' @param dbhsd dbh sd
#' @param height tree height in meters
#' @param hsd height sd
#' @returns  standard error
#' @references
# 'Eq. 4 in Chave et al., 2014 but simplied to power 1 not 0.9....
#' @note assumes allometric equation has no error, which is not correct...
#'
pro_error_vol <- function(dbh,dbhsd,height,hsd) {

  vol_expression <- expression(0.0673*(hc * dbhc^2)^0.976)
  hc <- c(height,hsd)
  dbhc <- c(dbh,dbhsd)
  df <- cbind(hc,dbhc)
  results <- propagate(expr = vol_expression, data = df, type = "stat", do.sim = TRUE, verbose = TRUE, nsim = 100000)
  return(results$sim)
}
