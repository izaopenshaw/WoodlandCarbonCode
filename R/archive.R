# VERSION 1
fc_agc <- function(spcode,dbh,height,returnv="AGC"){

  # Check arguments
  if(!is.numeric(dbh) || any(dbh<0))stop("dbh must be numeric and positive")
  if(!is.numeric(height) || any(height<0))stop("height must be numeric and positive")
  if (!is.character(spcode))stop("spcode must be a character")

  # Lookup species data from code
  rec <- sp_lookupdf[sp_lookupdf$short %in% spcode, ]

  # Check if all species code values were found
  not_found <- setdiff(spcode, rec$short)
  if (length(not_found) > 0) {warning("The following spcode values were not found: ", paste(not_found, collapse = ", "))}

  tarifflokupcode <- rec$single
  type <- rec$type

  # Get tariff number depending on broadleaf or conifer
  if (type == 'broadleaf'){
    tariff <- fc_broad_tariff(tarifflokupcode, height, dbh)
  } else if (type == "conifer") {
    tariff <- fc_con_tariff(tarifflokupcode, height, dbh)
  }

  mercvol <- fc_merchtreevol(tariff, dbh)          # Merchantable tree volume
  stemvol <- fc_treevol(mercvol, dbh)             # Stem volume
  stembiomass <- fc_woodbiomass(stemvol, rec$NSG) # Stem Biomass
  crownbiomass <- fc_crownbiomass(rec$Crown, dbh)  # Crown Biomass
  AGC <- (stembiomass + crownbiomass) * 0.5        # Total above ground carbon

  if(returnv == "AGC"){
    return(AGC)
  } else {
    rootbiomass <- fc_rootbiomass(rec$Root, dbh)   # Root Biomass
    return(list(sp_code = spcode, tariff = tariff, merc_vol = mercvol,
                stem_vol = stemvol, stem_biomass = stembiomass,
                crown_biomass = crownbiomass, root_biomass = rootbiomass, AGC = AGC))
  }
}
