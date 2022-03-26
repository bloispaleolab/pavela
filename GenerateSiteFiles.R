# Final script for generating individual site files for upload

# Load packages
options(scipen=999)
library(measurements)

# set path to google drive
path.to.google <- "/Volumes/GoogleDrive/My Drive/Neotoma Overall/Neotoma Vertebrates/Data/2. In progress/PaVeLA/PaVeLA-working/PaVeLA upload/"

# Read in the primary Sites file ----

# Create file for Site metadata tab ----
# WARNING: will need to regenerate LOCALIDA file with updated information
sites <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",", fileEncoding="UTF-8")

# Note: will create and save a single dataframe first, then export individual ones to each locality folder
    
    alternate <- read.delim('data/pavela_flat_files/NOMBRE ALTERNATIVO LOCALIDAD.txt', header=T, sep=",", fileEncoding="UTF-8") 
    
    ## Create a new 'sites' dataframe ----
    sites_n <- NULL
    sites_n$siteid <- sites$IDLOCALIDAD
    sites_n$sitename <- sites$NOMBRE_LOCALIDAD # will need to verify that spanish accents rendered correctly in Tilia
    sites_n$longitudeeast <- NA
    sites_n$latitudenorth <- NA
    sites_n$longitudewest <- NA
    sites_n$latitudesouth <- NA
    sites_n$altitude <- sites$ELEVACION
    sites_n$altitude[which(sites_n$altitude == 9999)] <- NA
    sites_n$area <- NA
    sites_n$sitedescription <- NA
    sites_n$notes <- NA
    
    sites_n <- as.data.frame(sites_n)
    
    ## Convert to lat/long in decimal degrees ----
    # latitude
    deg.lat.NA <- which(sites$LATITUD_GRADOS == '999')
    min.lat.NA <- which(sites$LATITUD_MINUTOS == '99')
    sec.lat.NA <- which(sites$LATITUD_SEGUNDOS == '99')
    deg.lat.real <- which(sites$LATITUD_GRADOS != '999')
    min.lat.real <- which(sites$LATITUD_MINUTOS != '99')
    sec.lat.real <- which(sites$LATITUD_SEGUNDOS != '99')
    
    # which sites have NA for degrees, minutes, & seconds
    lat.set1 <- intersect(intersect(deg.lat.NA, min.lat.NA), sec.lat.NA)
    
    # which sites have actual degrees, missing min and sec
    lat.set2 <- intersect(deg.lat.real, intersect(min.lat.NA, sec.lat.NA))
    
    # how many sites have real deg and minutes, but missing sec
    lat.set3 <- intersect(intersect(deg.lat.real, min.lat.real), sec.lat.NA)
    
    # how many sites have real data for deg, minutes, and sec
    lat.set4 <- intersect(intersect(deg.lat.real, min.lat.real), sec.lat.real)
    
    # Check: should be true
    (length(lat.set1)+length(lat.set2) + length(lat.set3) + length(lat.set4)) == nrow(sites)
    
    #longitude
    deg.long.NA <- which(sites$LONGITUD_GRADOS == '999')
    min.long.NA <- which(sites$LONGITUD_MINUTOS == '99')
    sec.long.NA <- which(sites$LONGITUD_SEGUNDOS == '99')
    deg.long.real <- which(sites$LONGITUD_GRADOS != '999')
    min.long.real <- which(sites$LONGITUD_MINUTOS != '99')
    sec.long.real <- which(sites$LONGITUD_SEGUNDOS != '99')
    
    # which sites have NA for degrees, minutes, & seconds. save the site index, not siteid, so can use next
    long.set1 <- intersect(intersect(deg.long.NA, min.long.NA), sec.long.NA)
    
    # which sites have actual degrees, missing min and sec
    long.set2 <- intersect(deg.long.real, intersect(min.long.NA, sec.long.NA))
    
    # how many sites have real deg and minutes, but missing sec
    long.set3 <- intersect(intersect(deg.long.real, min.long.real), sec.long.NA)
    
    # how many sites have real data for deg, minutes, and sec
    long.set4 <- intersect(intersect(deg.long.real, min.long.real), sec.long.real)
    
    # Check: should be true
    (length(long.set1)+length(long.set2) + length(long.set3) + length(long.set4)) == nrow(sites)
    
    lat_dec <- long_dec <- vector(length=nrow(sites))
    
    # convert set1 sites with all NA data
    lat_dec[lat.set1] <- NA
    long_dec[long.set1] <- NA
    
    # convert set2 sites with just degree data
    lat_DMS <- paste(sites$LATITUD_GRADOS, 0, 0)
    long_DMS <- paste(sites$LONGITUD_GRADOS, 0, 0)
    lat_dec[lat.set2] <- conv_unit(lat_DMS[lat.set2], from = "deg_min_sec", to = "dec_deg")
    long_dec[long.set2] <- conv_unit(long_DMS[long.set2], from = "deg_min_sec", to = "dec_deg")
    long_dec[long.set2] <- -long_dec[long.set2] 
    # note, no longitudes in set2
    
    # convert set3 sites with just degree and minute data
    lat_DMS <- paste(sites$LATITUD_GRADOS, sites$LATITUD_MINUTOS, 0)
    long_DMS <- paste(sites$LONGITUD_GRADOS, sites$LONGITUD_MINUTOS, 0)
    
    lat_dec[lat.set3] <- conv_unit(lat_DMS[lat.set3], from = "deg_min_sec", to = "dec_deg")
    long_dec[long.set3] <- conv_unit(long_DMS[long.set3], from = "deg_min_sec", to = "dec_deg")
    long_dec[long.set3] <- -long_dec[long.set3] 
    
    # convert set4 sites with complete coordinate data
    lat_DMS <- paste(sites$LATITUD_GRADOS, sites$LATITUD_MINUTOS, sites$LATITUD_SEGUNDOS)
    long_DMS <- paste(sites$LONGITUD_GRADOS, sites$LONGITUD_MINUTOS, sites$LONGITUD_SEGUNDOS)
    
    lat_dec[lat.set4] <- conv_unit(lat_DMS[lat.set4], from = "deg_min_sec", to = "dec_deg")
    long_dec[long.set4] <- conv_unit(long_DMS[long.set4], from = "deg_min_sec", to = "dec_deg")
    long_dec[long.set4] <- -long_dec[long.set4] 
    
    lat_dec <- as.numeric(lat_dec)
    long_dec <- as.numeric(long_dec)
    
    sites_n$longitudeeast <- long_dec
    sites_n$latitudenorth <- lat_dec
    sites_n$longitudewest <- long_dec
    sites_n$latitudesouth <- lat_dec
    
    ## Create the site notes column ----
    ## Merge Numero_Mapa and Comentarios to create a new notes column
    notes <- rep(NA, length=nrow(sites_n))
    
    # sites without any comments in either numero_mapa or comentario
    # temp <- intersect(which(sites$NUMERO_MAPA == "ND"), which(sites$COMENTARIOS == "ND"))
    # notes[temp] <- ""
    
    # sites with comments in cometario but no comments in numero_mapa
    temp <- intersect(which(sites$NUMERO_MAPA == "ND"), which(sites$COMENTARIOS != "ND"))
    notes[temp] <- sites$COMENTARIOS[temp]
    
    # sites with comments in numero_mapa but no comments in cometario
    temp <- intersect(which(sites$NUMERO_MAPA != "ND"), which(sites$COMENTARIOS == "ND"))
    notes[temp] <- paste0("Original NUMERO_MAPA = ", sites$NUMERO_MAPA[temp])
    
    # sites with comments in both cometario and numero_mapa
    temp <- intersect(which(sites$NUMERO_MAPA != "ND"), which(sites$COMENTARIOS != "ND"))
    notes[temp] <- paste0(sites$COMENTARIOS[temp], "; Original NUMERO_MAPA = ", sites$NUMERO_MAPA[temp])
    
    # now, add the alternate names to the notes.  
    for (i in 1:nrow(sites_n)){
      temp <- which(alternate$IDLOCALIDA == sites_n$siteid[i])
      if (any(alternate$NOMBRE_ALTERNATIVO[temp] != "ND")){
        notes[i] <- paste0(notes[i], ". Alternate site names: ", paste(alternate$NOMBRE_ALTERNATIVO[temp], collapse="; "))  
      }
    }
    
    sites_n$notes <- notes
    
    ## save the new sites dataframe ----
    write.table(sites_n, file = paste0(path.to.google, "Site files/SITES_in_neotoma_format.txt"), row.names = F, sep="\t")
    
    ## save an individual sites file to each folder ----
    for (i in 1:nrow(sites_n)){
      write.table(sites_n[i,], file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/Site_Info_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
    }


# Create file for Collection unit metadata tab and Deposit Analysis Units ----
    sites <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",", fileEncoding="UTF-8")
    sites_n <- read.delim(file = paste0(path.to.google, "Site files/SITES_in_neotoma_format.txt"), header=T, sep="\t")
    
    # Read in relevant files (in addition to primary 'sites' file on line 11)
    deposit <- read.delim('data/pavela_flat_files/DEPOSITO.txt', header=T, sep="\t", fileEncoding="UTF-8")
    ambien <- read.delim('data/pavela_flat_files/T_AMBIEN.txt', header=T, sep="\t", fileEncoding="UTF-8")
    sistem <- read.delim('data/pavela_flat_files/T_SISTEM.txt', header=T, sep="\t", fileEncoding="UTF-8")
    facies <- read.delim('data/pavela_flat_files/T_FACIES.txt', header=T, sep=",", fileEncoding="UTF-8")
    contam <- read.delim('data/pavela_flat_files/T_CONTAM.txt', header=T, sep=",", fileEncoding="UTF-8")
    recupe <- read.delim('data/pavela_flat_files/T_RECUPE.txt', header=T, sep=",", fileEncoding="UTF-8")
    
    ## Start adding values for each site
    for (i in 1:nrow(sites)){
      # set siteid
      siteid <- sites$IDLOCALIDAD[i]
      
    ## Create overall Analysis Units file ----
      # Note 1: SISTEMA_DEPOSICION & AMBIENTE_DEPOSICION together create depositional environment. 
      # If AMBIENTE_DEPOSICION == "ND", then choose name from SISTEMA_DEPOSICION. 
      # Otherwise, choose name from AMBIENTE_DEPOSICION
      # Note 2: T_FACIES, T_CONTAM are associated with each analysis unit
      # Note 3: METODO_RECUPERACION is technically an attribute of samples, but best to store it with analysis units for now
      
      # this finds all analysis units within the deposit file
      rows <- which(deposit$IDLOCALIDAD == siteid) 
      aus.deposit <- deposit[rows,-1]
      
      # replace FaciesIDs with names
      aus.deposit$IDFACIES <- facies[charmatch(aus.deposit$IDFACIES, facies[,1]), 2]
      
      # replace ContamIDs with names
      aus.deposit$IDCONTAMINACION <- contam[charmatch(aus.deposit$IDCONTAMINACION, contam[,1]), 2]

      # replace RecoverMethod with names
      aus.deposit$METODO_RECUPERACION <- recupe[charmatch(aus.deposit$METODO_RECUPERACION, recupe[,1]), 2]
      
      # replace SISTEMA_DEPOSICION with names
      aus.deposit$SISTEMA_DEPOSICION <- sistem[charmatch(aus.deposit$SISTEMA_DEPOSICION, sistem[,1]), 1]
      
      # replace AMBIENTE_DEPOSICION with names
      aus.deposit$AMBIENTE_DEPOSICION <- ambien[charmatch(aus.deposit$AMBIENTE_DEPOSICION, ambien[,1]), 1]
     
      colnames(aus.deposit) <- c("AnalysisUnitName", "RecoveryMethod", "DepositionSystem", "DepositionEnvt", "Facies", "Contamination")
      
      ## write analysis.unit file to folders ----
      write.table(aus.deposit, file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/AnalysisUnits_Deposit_Info_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
      
  ## Now move on to Collection Unit information ----
      ## Create a new 'collunit' dataframe ----
      collunit <- NULL
      collunit$siteid <- sites$IDLOCALIDAD
      collunit$handle <- NA
      collunit$collunittype <- "Unknown" # will change manually if we can determine this upon site validation
      collunit$collunitname <- NA
      collunit$depenvt <- NA
      collunit$location <- NA
      collunit$notes <- NA
      
      # set handle 
      collunit$handle <- paste0("PaVeLA_", sites$IDLOCALIDAD[i])
      
      ## determine depositional environment ----

        # create depositional environment for collunits
        depenvt <- paste(aus.deposit$DepositionSystem, aus.deposit$DepositionEnvt, sep="; ")
        
        depenvt <- unique(depenvt)
        if (length(depenvt)>1){
          collunit$depenvt[i] <- "AnalysisUnits_Deposit_Info"
        }else{
          collunit$depenvt[i] <- depenvt
        }
      
      ## save collection unit info to each folder
      write.table(collunit, file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/CollectionUnit_Info_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
 } # close sites
    
    
# Create file for other Analysis units ----
    sites <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",", fileEncoding="UTF-8")
    sites_n <- read.delim(file = paste0(path.to.google, "Site files/SITES_in_neotoma_format.txt"), header=T, sep="\t")
    
    # This is ultimately added to the 'Data' tab in Neotoma
   
    ## Find all possible 'analysisunitname' ----
    # Note: some analysis unit names are in deposito, some in edadabs, some in edadrel

    ### Read in relevant files
    deposit <- read.delim('data/pavela_flat_files/DEPOSITO.txt', header=T, sep="\t", fileEncoding="UTF-8")
    abs <- read.delim('data/pavela_flat_files/EDADABS.txt', header=T, sep=",", fileEncoding="UTF-8")
    rel <- read.delim('data/pavela_flat_files/EDADREL.txt', header=T, sep=",", fileEncoding="UTF-8")
    
    ## Start accumulating all possible analysis unit names for each site
    for (i in 1:nrow(sites)){
      # set siteid
      siteid <- sites$IDLOCALIDAD[i]
      
      ## analysis unit names in fauna ----
      # start with fauna, since this may contain the most precise info linked to specimens
      rows <- which(fauna$IDLOCALIDAD == siteid) 
      names.fauna <- unique(fauna$UNIDAD_DE_ANALISIS[rows])

      ## analysis unit names in edadabs ----
      rows <- which(abs$IDLOCALIDAD == siteid) 
      names.edadabs <- unique(abs$UNIDAD_DE_ANALISIS[rows])
      found.in.edadabs <- rep("Y", length(names.edadabs))
      
      ## analysis unit names in edadrel ----
      rows <- which(rel$IDLOCALIDAD == siteid) 
      names.edadrel <- unique(rel$UNIDAD_DE_ANALISIS[rows])
      found.in.edadrel <- rep("Y", length(names.edadrel))
      
      # merge
      AnalysisUnitName <- unique(c(names.fauna, names.edadabs, names.edadrel))
      found.in.fauna <- found.in.edadabs <- found.in.edadrel <- vector(length=length(AnalysisUnitName))
      found.in.fauna[match(names.fauna, AnalysisUnitName)] <- TRUE
      found.in.edadabs[match(names.edadabs, AnalysisUnitName)] <- TRUE
      found.in.edadrel[match(names.edadrel, AnalysisUnitName)] <- TRUE
      
      aus <- NULL
      aus$AnalysisUnitName <- AnalysisUnitName
      aus$found.in.fauna <- found.in.fauna
      aus$found.in.edadabs <- found.in.edadabs
      aus$found.in.edadrel <- found.in.edadrel
      aus <- as.data.frame(aus)
      
      # write analysis.unit file to folders
      write.table(aus, file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/AnalysisUnits_fauna_ages_Info_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
    }  
      
# create file for Dataset metadata tab
# create file for Geochronology metadata tab
# Gather any relevant information on Relative Ages or Cultural associations

# Create files for site taxa ----
    sites <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",", fileEncoding="UTF-8")
    sites_n <- read.delim(file = paste0(path.to.google, "Site files/SITES_in_neotoma_format.txt"), header=T, sep="\t")
    
    fauna <- read.delim('data/pavela_flat_files/FAUNA.txt', header=T, sep=",", fileEncoding="UTF-8")
    especies <- read.delim('data/pavela_flat_files/ESPECIES.txt', header=T, sep=",", fileEncoding="UTF-8")

for (i in 1:nrow(sites)){

  # set siteid
  siteid <- sites$IDLOCALIDAD[i]
  site.fauna <- fauna[which(fauna$IDLOCALIDAD == siteid),]
  
  # create basic Data dataframe
  # colnames
  anUnits <- unique(site.fauna$UNIDAD_DE_ANALISIS)
  # rownames - use the valid species here
  validtax <- unique(site.fauna$IDESPECIEVALIDA)
  
  # Find any NA values in the valid taxa IDs and remove them from both validtax and site.fauna
  trouble.sites <- NULL
  if (any(is.na(validtax))){
    print(paste0("WARNING! Not all taxa are valid! i = ", i, "; siteid =", siteid))
    trouble.sites <- c(trouble.sites, siteid)
    validtax <- validtax[-which(is.na(validtax))]
    site.fauna <- site.fauna[-which(is.na(site.fauna$IDESPECIEVALIDA)),]
  }

  
  # create primary dataframe for the site of all taxa
  df <- as.data.frame(matrix(data=NA, nrow=length(validtax), ncol=length(anUnits)))
  colnames(df) <- anUnits
  rownames(df) <- validtax
  
  
  # First, tally up all the different data units represented at this site: PA, NISP, MNI 
  # if multiple data types, will need to create separate dataframes for each.
  possible.units <- NULL
  if (any(unique(site.fauna$NMI) != 999999999)){possible.units <- c(possible.units, "MNI")}
  if ( (length(unique(site.fauna$NUM_EJEMPLARES))>=1) & (any(unique(site.fauna$NUM_EJEMPLARES)==999999999)) ){possible.units <- c(possible.units, "NISP", "PA")} # both NISP and PA data (total length MUST be >1)
  if ( (length(unique(site.fauna$NUM_EJEMPLARES)) >= 1) & (all(unique(site.fauna$NUM_EJEMPLARES)!=999999999)) ){possible.units <- c(possible.units, "NISP")} # at least one unique value, but no 999999999s
  if ( all(unique(site.fauna$NUM_EJEMPLARES)==999999999) ){possible.units <- c(possible.units, "PA")} # only one unique value, and it's 999999999
  
  # create subsets of data for each data type
  site.fauna.pa <- site.fauna[which(site.fauna$NUM_EJEMPLARES == 999999999),]
  site.fauna.nisp <- site.fauna[which(site.fauna$NUM_EJEMPLARES != 999999999),]
  site.fauna.mni <- site.fauna[which(site.fauna$NMI != 999999999),]
  df.PA <- df.NISP <- df.MNI <- df
    
  ## create the pa dataframe ----
  if (nrow(site.fauna.pa)>0){ 
    for (j in 1:length(anUnits)){
      temp <- site.fauna.pa[which(site.fauna.pa$UNIDAD_DE_ANALISIS == anUnits[j]), ]
      subtax <- unique(temp$IDESPECIEVALIDA)
      df.PA[match(subtax, validtax), j] <- 'present' # mark taxa as present in this analysis unit
      rm(temp); rm(subtax)
    }
  }
  
  ## create the nisp dataframe ----
  # need to tally up duplicated taxa here
  if (nrow(site.fauna.nisp)>0){ 
    for (j in 1:length(anUnits)){
      # create temporary dataframe
      temp <- site.fauna.nisp[which(site.fauna.nisp$UNIDAD_DE_ANALISIS == anUnits[j]), ]
      
      if (any(duplicated(temp$IDESPECIEVALIDA)==TRUE)){ # if there are any duplicates
        # scroll through each species
        for (k in 1:nrow(df.NISP)){
          temp.tax <- rownames(df.NISP)[k]
          if (any(temp$IDESPECIEVALIDA == temp.tax)){
            df.NISP[k,j] <- sum(temp[which(temp$IDESPECIEVALIDA == temp.tax), 'NUM_EJEMPLARES'])
          }
        }
      }else{
        df.NISP[match(temp$IDESPECIEVALIDA, validtax), j] <- temp[, 'NUM_EJEMPLARES']
      }
      rm(temp)
    }
  }
  
  ## create the mni dataframe ----
  # need to tally up duplicated taxa here
  if (nrow(site.fauna.mni)>0){ 
    for (j in 1:length(anUnits)){
      # create temporary dataframe
      temp <- site.fauna.mni[which(site.fauna.mni$UNIDAD_DE_ANALISIS == anUnits[j]), ]
      
      if (any(duplicated(temp$IDESPECIEVALIDA)==TRUE)){ # if there are any duplicates
        # scroll through each species
        for (k in 1:nrow(df.MNI)){
          temp.tax <- rownames(df.MNI)[k]
          if (any(temp$IDESPECIEVALIDA == temp.tax)){
            df.MNI[k,j] <- sum(temp[which(temp$IDESPECIEVALIDA == temp.tax), 'NMI'])
          }
        }
      }else{
        df.MNI[match(temp$IDESPECIEVALIDA, validtax), j] <- temp[, 'NMI']
      }
      rm(temp)    
    }
  }
  
## Write site data ---- 
# write the NISP data for a site
  if (any(!is.na(df.NISP))){ # only save data if some data are stored
    # remove any rows with all NA
    df.NISP <- df.NISP[-which(apply(df.NISP, 1, function(x) all(is.na(x)))),]
    
    element <- rep("bone/tooth", nrow(df.NISP))
    units <- rep("NISP", nrow(df.NISP))
    name <- especies[match(rownames(df.NISP), especies$IDESPECIEVALIDA),'TAXON_VALIDO']
    
    df.NISP <- cbind(name, element, units, df.NISP)
    
    write.table(df.NISP, file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/Data_NISP_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
  }
  
# write the MNI data for a site
  if (any(!is.na(df.MNI))){
    # remove any rows with all NA
    df.MNI <- df.MNI[-which(apply(df.MNI, 1, function(x) all(is.na(x)))),]
    
    element <- rep("bone/tooth", nrow(df.MNI))
    units <- rep("MNI", nrow(df.MNI))
    name <- especies[match(rownames(df.MNI), especies$IDESPECIEVALIDA),'TAXON_VALIDO']
    
    df.MNI <- cbind(name, element, units, df.MNI)
    
    write.table(df.MNI, file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/Data_MNI_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
  }
  
  # write the PA data for a site
  if (any(!is.na(df.PA))){
    # remove any rows with all NA
    df.PA <- df.PA[-which(apply(df.PA, 1, function(x) all(is.na(x)))),]
    
    element <- rep("bone/tooth", nrow(df.PA))
    units <- rep("present/absent", nrow(df.PA))
    name <- especies[match(rownames(df.PA), especies$IDESPECIEVALIDA),'TAXON_VALIDO']
    
    df.PA <- cbind(name, element, units, df.PA)
    
    write.table(df.PA, file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/Data_PA_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
  }
  
} # close site loop
  
 

RPUBLOC <- read.delim('data/pavela_flat_files/RPUBLOC.txt', header=T, sep=",", fileEncoding="UTF-8")
pubs <- read.delim('data/pavela_flat_files/PUBLIC.txt', header=T, sep=",", fileEncoding="UTF-8")



## OLD:
# Just do this once:

# # Set path to Google Drive
# path.to.google <- "/Volumes/GoogleDrive/My Drive/Neotoma Overall/Neotoma Vertebrates/Data/2. In progress/PaVeLA/PaVeLA-working/PaVeLA upload/"
# 
# sites <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",", fileEncoding="UTF-8")
# 
# # create folders in Google Drive
# for (i in 1:nrow(sites)){
#   dir.create(paste0(path.to.google, 'Site files/IDLOCALIDAD_', sites$IDLOCALIDAD[i]), showWarnings = TRUE, recursive = FALSE)
# }

