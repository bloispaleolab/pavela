# Clean PaVeLA data to match FAUNMAP/Neotoma ----

# Load libraries
install.packages("measurements") 
library(measurements)

# Start with Sites ----
SITES <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",")

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

> print(c(lat_d, lng_d))
## Convert to lat/long in decimal degrees

## Merge Numero_Mapa into Comentarios


# Create Collection Units file ----

# if LOCALIDA$IDPRECISION == CP, then store GPS coords in Collection units as well

lat_d <- char2dms(lat, chd='d', chm='m', chs='s') %>% as.numeric()
> lng_d <- char2dms(lng, chd='d', chm='m', chs='s') %>% as.numeric()

> print(c(lat_d, lng_d))


## Deal with ESTADO and MUNICIPI - geopolitical
