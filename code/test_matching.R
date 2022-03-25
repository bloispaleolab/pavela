# test the match from deposito and edadabs
sites <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep=",", fileEncoding="UTF-8")
deposit <- read.delim('data/pavela_flat_files/DEPOSITO.txt', header=T, sep="\t", fileEncoding="UTF-8")
abs <- read.delim('data/pavela_flat_files/EDADABS.txt', header=T, sep=",", fileEncoding="UTF-8")
rel <- read.delim('data/pavela_flat_files/EDADREL.txt', header=T, sep=",", fileEncoding="UTF-8")
RPUBLOC <- read.delim('data/pavela_flat_files/RPUBLOC.txt', header=T, sep=",", fileEncoding="UTF-8")
pubs <- read.delim('data/pavela_flat_files/PUBLIC.txt', header=T, sep=",", fileEncoding="UTF-8")
especies <- read.delim('data/pavela_flat_files/ESPECIES.txt', header=T, sep=",", fileEncoding="UTF-8")
fauna <- read.delim('data/pavela_flat_files/FAUNA.txt', header=T, sep=",", fileEncoding="UTF-8")

# how many sites have coordinates?
sites$with.coords <- NULL

# of the sites_with_coords, how many have radiocarbon dates?
sites$num.abs.dates <- NULL

# of the sites_with_coords, how many have relative ages?
sites$num.rel.ages <- NULL

# sites with primary publications
sites$primary.pubs <- NULL

# number of taxa at a site
sites$num.taxa <- NULL

for (i in 1:nrow(sites)){
  siteid <- sites$IDLOCALIDAD[i]
  if (sites$LATITUD_GRADOS[i] == 999){
    sites$with.coords[i] <- "N"
    }else{
    sites$with.coords[i] <- "Y"
    } 
  # find abs dates
  if (length(which(abs$IDLOCALIDAD == siteid)) > 0){
    sites$num.abs.dates[i] <- length(which(abs$IDLOCALIDAD == siteid))
  }else{
    sites$num.abs.dates[i] <- 0
  }
  # find rel ages
  if (length(which(rel$IDLOCALIDAD == siteid)) > 0){
    sites$num.rel.ages[i] <- length(which(rel$IDLOCALIDAD == siteid))
  }else{
    sites$num.rel.ages[i] <- 0
  }
  
  # find types of publications
  pubids <- RPUBLOC[which(RPUBLOC$IDLOCALIDAD == siteid), 'IDPUBLICACION']
  sites$primary.pubs[i] <- any(pubs[pubids,c('IDFUENTE')] == 1)
  
  # tally number of species at a site
  sites$num.taxa[i] <- length(unique(fauna[which(fauna$IDLOCALIDAD == siteid),'IDESPECIEVALIDA']))
}

spatial.rank <- vector(length=nrow(sites))
spatial.rank[which(sites$with.coords == "Y")] <- 1
spatial.rank[which(sites$with.coords == "N")] <- 2

temporal.rank <- vector(length=nrow(sites))
temporal.rank[which(sites$num.abs.dates > 0)] <- 1
temporal.rank[intersect(which(sites$num.abs.dates == 0), which(sites$num.rel.ages >0))] <- 2
temporal.rank[intersect(which(sites$num.abs.dates == 0), which(sites$num.rel.ages == 0))] <- 3

material.rank <- vector(length=nrow(sites))
material.rank[which(sites$primary.pubs == "TRUE")] <- 1
material.rank[which(sites$primary.pubs == "FALSE")] <- 2

taxa.rank <- vector(length=nrow(sites))
taxa.rank[which(sites$num.taxa > 9)] <- 1
taxa.rank[intersect(which(sites$num.taxa < 10), which(sites$num.taxa > 4))] <- 2
taxa.rank[intersect(which(sites$num.taxa < 5), which(sites$num.taxa > 0))] <- 3
taxa.rank[which(sites$num.taxa == 0)] <- 4

sites <- cbind(sites, spatial.rank, temporal.rank, material.rank, taxa.rank)
write.table(sites, file="data/pavela_sites.txt", row.names=F, sep="\t")


sites.with.coords <- which(sites$LATITUD_GRADOS != 999)
sites.with.abs.dates <- which(sites$num.abs.dates > 0)
sites.with.primary.lit <- which(sites$primary.pubs == "TRUE")

dim(sites[intersect(intersect(sites.with.coords, sites.with.abs.dates), sites.with.primary.lit),])




# are there any sites with degrees, but NA for minutes or seconds?
deg.lat <- which(sites$LATITUD_GRADOS == '999')
min.lat <- which(sites$LATITUD_MINUTOS == '99')
sec.lat <- which(sites$LATITUD_SEGUNDOS == '99')
deg.lat.real <- which(sites$LATITUD_GRADOS != '999')
min.lat.real <- which(sites$LATITUD_MINUTOS != '99')
sec.lat.real <- which(sites$LATITUD_SEGUNDOS != '99')

# how many sites have NA for degrees, minutes, seconds
length(intersect(intersect(deg.lat, min.lat), sec.lat))

# how many sites have real degrees, missing min and sec
length(intersect(deg.lat.real, intersect(min.lat, sec.lat)))

# how many sites have real deg and minutes, but missing sec
length(intersect(intersect(deg.lat.real, min.lat.real), sec.lat))





for (j in 1:length(sites)){
  
}


# old

# total localities
locality <- sites$IDLOCALIDAD
number_deposit_AU <- NULL
number_abs_AU <- NULL
number_rel_AU <- NULL


for (i in 1:10){#length(locality)){
  print(cat("Site Name: ", sites$NOMBRE_LOCALIDAD[i]))
  number_deposit_AU[i] <- length(which(deposit$IDLOCALIDAD == i))
  print(cat("Deposit AU Names: ", deposit[which(deposit$IDLOCALIDAD == i),'UNIDAD_DE_ANALISIS']))
  number_abs_AU[i] <- length(which(abs$IDLOCALIDAD == i))
  print(cat("Abs AU Names: ", abs[which(abs$IDLOCALIDAD == i),'UNIDAD_DE_ANALISIS']))
  number_rel_AU[i] <- length(which(rel$IDLOCALIDAD == i))
  print(cat("Rel AU Names: ", rel[which(rel$IDLOCALIDAD == i),'UNIDAD_DE_ANALISIS']))
}

comparison <- as.data.frame(cbind(locality, number_deposit_AU, number_abs_AU, number_rel_AU))
#comparison$difference <- comparison$number_deposit_AU - comparison$number_abs_AU


