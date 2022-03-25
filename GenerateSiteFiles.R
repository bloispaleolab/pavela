# Final script for generating individual site files for upload

# Load packages
library(measurements)

# set path to google drive
path.to.google <- "/Volumes/GoogleDrive/My Drive/Neotoma Overall/Neotoma Vertebrates/Data/2. In progress/PaVeLA/PaVeLA-working/PaVeLA upload/"

# Read in the primary Sites file ----
# WARNING: will need to regenerate LOCALIDA file with updated information
sites <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",", fileEncoding="UTF-8")

# Create file for Site metadata tab ----
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


# Create file for Collection unit metadata tab ----

    # Notes: # Depositional Environment
    # SISTEMA_DEPOSICION & AMBIENTE_DEPOSICION together create depositional environment.
    # If AMBIENTE_DEPOSICION == "ND", then choose name from SISTEMA_DEPOSICION
    # Otherwise, choose name from AMBIENTE_DEPOSICION
    
    # Read in relevant files (in addition to primary 'sites' file on line 11)
    deposit <- read.delim('data/pavela_flat_files/DEPOSITO.txt', header=T, sep="\t", fileEncoding="UTF-8")
    ambien <- read.delim('data/pavela_flat_files/T_AMBIEN.txt', header=T, sep="\t", fileEncoding="UTF-8")
    sistem <- read.delim('data/pavela_flat_files/T_SISTEM.txt', header=T, sep="\t", fileEncoding="UTF-8")
     
    ## Create a new 'collunit' dataframe ----
    collunit <- NULL
    collunit$siteid <- sites$IDLOCALIDAD
    collunit$handle <- NA
    collunit$collunittype <- "Unknown" # will change manually if we can determine this upon site validation
    collunit$collunitname <- NA
    collunit$depenvt <- NA
    collunit$location <- NA
    collunit$notes <- NA
    
    ## Start adding values for each site
    for (i in 1:nrow(sites)){
      # set siteid
      siteid <- sites$IDLOCALIDAD[i]
      
      # set handle 
      collunit$handle <- paste0("PaVeLA_", sites$IDLOCALIDAD[i])
      
      ## determine depositional environment ----
      # Note: SISTEMA_DEPOSICION & AMBIENTE_DEPOSICION together create depositional environment. If AMBIENTE_DEPOSICION == "ND", then choose name from SISTEMA_DEPOSICION. Otherwise, choose name from AMBIENTE_DEPOSICION
      
      # this finds all analysis units within the deposit 
      # (note, there are other analysis units in other spreadsheets though)
      rows <- which(deposit$IDLOCALIDAD == siteid) 

        # create depositional environment
        depenvt <- paste(
          sistem[charmatch(deposit$SISTEMA_DEPOSICION[rows], sistem[,1]),1], # all sistema_deposicion
          ambien[charmatch(deposit$AMBIENTE_DEPOSICION[rows], ambien[,1]),1], # all ambient_deposicion
          sep="; ")
        
        if (length(rows)>1){
          # bind rows with analysis unit names
          depenvt <- cbind(deposit$UNIDAD_DE_ANALISIS[rows], depenvt)
          colnames(depenvt)[1] <- 'UNIDAD_DE_ANALISIS'
          
          # write the full dep environment files to the folder and add a note in the collunit$depenvt column
          write.table(depenvt, file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/DepEnvt_Info_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
          collunit$depenvt[i] <- "Consult DepEnvt_Info file"
          
      }else{
        collunit$depenvt[i] <- depenvt
      }
      ## save collection unit info to each folder
      write.table(collunit, file = paste0(path.to.google, "Site files/IDLOCALIDAD_", sites_n$siteid[i], "/CollectionUnit_Info_", sites_n$siteid[i], ".txt"), row.names = F, sep="\t")
    }
    
# Create file for Analysis units ----
    # This is ultimately added to the 'Data' tab in Neotoma
    
    ## Find all possible 'analysisunitname' ----
    
    
abs <- read.delim('data/pavela_flat_files/EDADABS.txt', header=T, sep=",", fileEncoding="UTF-8")
rel <- read.delim('data/pavela_flat_files/EDADREL.txt', header=T, sep=",", fileEncoding="UTF-8")




RPUBLOC <- read.delim('data/pavela_flat_files/RPUBLOC.txt', header=T, sep=",", fileEncoding="UTF-8")
pubs <- read.delim('data/pavela_flat_files/PUBLIC.txt', header=T, sep=",", fileEncoding="UTF-8")


# create file for Dataset metadata tab

# create file for Geochronology metadata tab

# Gather any relevant information on Relative Ages or Cultural associations

# create file on taxon list
fauna <- read.delim('data/pavela_flat_files/FAUNA.txt', header=T, sep=",", fileEncoding="UTF-8")
especies <- read.delim('data/pavela_flat_files/ESPECIES.txt', header=T, sep=",", fileEncoding="UTF-8")



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

