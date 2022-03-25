# Clean PaVeLA data to match FAUNMAP/Neotoma ----

# Notes: first open the flat files in BBEdit and Zap Gremlins

# Load libraries
install.packages("measurements") 
library(measurements)

# Start with Sites ----
## NOTE: Still need to resolve sitegeopolitical - ESTADO and MUNICIPI

# load pavela data
SITES <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",", fileEncoding="UTF-8")

# Create a new 'sites' dataframe
sites_n <- NULL
sites_n$siteid <- SITES$IDLOCALIDAD
sites_n$sitename <- SITES$NOMBRE_LOCALIDAD
sites_n$longitudeeast <- NA
sites_n$latitudenorth <- NA
sites_n$longitudewest <- NA
sites_n$latitudesouth <- NA
sites_n$altitude <- SITES$ELEVACION
sites_n$area <- NA
sites_n$sitedescription <- NA
sites_n$notes <- NA

## Convert to lat/long in decimal degrees
# Note, needs to be modified because some sites have 999 for only the seconds, or minutes and seconds.
# see test matching code for information on this.
lat_d <- long_d <- vector(length=nrow(SITES))
lat_DMS <- paste(SITES$LATITUD_GRADOS, SITES$LATITUD_MINUTOS, SITES$LATITUD_SEGUNDOS)
long_DMS <- paste(SITES$LONGITUD_GRADOS, SITES$LONGITUD_MINUTOS, SITES$LONGITUD_SEGUNDOS)

lat_d[which(lat_DMS == "999 99 99")] <- 999
lat_d[which(lat_DMS != "999 99 99")] <- conv_unit(lat_DMS[which(lat_DMS != "999 99 99")], from = "deg_min_sec", to = "dec_deg")

long_d[which(long_DMS == "999 99 99")] <- 999
long_d[which(long_DMS != "999 99 99")] <- conv_unit(long_DMS[which(long_DMS != "999 99 99")], from = "deg_min_sec", to = "dec_deg")

lat_d <- as.numeric(lat_d)
long_d <- as.numeric(long_d)
long_d[which(long_d != 999)] <- -long_d[which(long_d != 999)]

sites_n$longitudeeast <- long_d
sites_n$latitudenorth <- lat_d
sites_n$longitudewest <- long_d
sites_n$latitudesouth <- lat_d

## Merge Numero_Mapa and Comentarios to create a new notes column
notes <- NULL

# sites without any comments in either numero_mapa or comentario
temp <- intersect(which(SITES$NUMERO_MAPA == "ND"), which(SITES$COMENTARIOS == "ND"))
notes[temp] <- NA

# sites with comments in cometario but no comments in numero_mapa
temp <- intersect(which(SITES$NUMERO_MAPA == "ND"), which(SITES$COMENTARIOS != "ND"))
notes[temp] <- SITES$COMENTARIOS[temp]

# sites with comments in numero_mapa but no comments in cometario
temp <- intersect(which(SITES$NUMERO_MAPA != "ND"), which(SITES$COMENTARIOS == "ND"))
notes[temp] <- paste0("Original NUMERO_MAPA = ", SITES$NUMERO_MAPA[temp])

# sites with comments in both cometario and numero_mapa
temp <- intersect(which(SITES$NUMERO_MAPA != "ND"), which(SITES$COMENTARIOS != "ND"))
notes[temp] <- paste0(SITES$COMENTARIOS[temp], "; Original NUMERO_MAPA = ", SITES$NUMERO_MAPA[temp])

sites_n$notes <- notes

# now, add the alternate names to the notes.  
## Note, Jessica needs to finish this later.
alternate <- read.delim('data/pavela_flat_files/NOMBRE ALTERNATIVO LOCALIDAD.txt', header=T, sep=",", fileEncoding="UTF-8") 

for (i in 1:length(sites_n)){
 temp <- which(alternate$IDLOCALIDA == sites_n$siteid[i])
 alternate$NOMBRE_ALTERNATIVO[temp]
 
 #sites_n$notes[i] <- paste0(sites_n$notes[i], )
}

# convert sites and save
sites_n <- as.data.frame(sites_n)
write.table(sites_n, file = "data/neotoma_files/sites.txt", row.names = F, sep="\t")

# Create Collection Units file ----

collectionunits <- NULL
collectionunits$siteid <- sites_n$siteid
collectionunits$collectionunitid <- sites_n$siteid
collectionunits$handle (# Handle <- paste0("FM",MachineNumber))
collectionunits$colltypeid
collectionunits$depenvtid
collectionunits$collunitname
collectionunits$colldate
collectionunits$gpslatitude
collectionunits$gpslongitude
collectionunits$gpserror
collectionunits$substrateid
collectionunits$notes

# if LOCALIDA$IDPRECISION == CP, then store GPS coords in Collection units as well

# Depositional Environment
# SISTEMA_DEPOSICION & AMBIENTE_DEPOSICION together create depositional environment.
# If AMBIENTE_DEPOSICION == "ND", then choose name from SISTEMA_DEPOSICION
# Otherwise, choose name from AMBIENTE_DEPOSICION



# Create Analysis Units file ----
analysisunits$collectionunitid <- collectionunits$collectionunitid
analysisunits$analysisunitid <- 
analysisunits$analysisunitname
analysisunits$depth
analysisunits$thickness
analysisunits$faciesid
analysisunits$mixed
analysisunits$notes 







