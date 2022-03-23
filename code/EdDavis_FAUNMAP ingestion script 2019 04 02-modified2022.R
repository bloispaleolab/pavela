# Script for ingesting FAUNMAP II data into data.frames 
# The data.frame then can be turned into tilia XML save files:

#clear teh memories!
rm(list = ls())

# Required package that might give problems is rJava.
# If you recieve an JAVA_HOME error it means that you need an updated 64 bit version of Java
# to run the XLConnect package.
# options(java.parameters = "-Xmx4g")
# install.packages('rJava')
# library(XLConnect)
# library(plyr)
library(dplyr)
library(xml2)
library(XML)

# set path to pavela

#make the data frames have strings, not factors!!!
options(stringsAsFactors = FALSE)

# the function below requires updated version of R and an updated 64 bit 
# version of java. Check to see what version of R you have.

# R.version

#first lets make the file name into a variable
P <- 'FAUNMAP for ingestion.xlsx'

# be sure to set your working dir to the excel file location

#Load the whole Workbook!
Fp_wbk <- loadWorkbook(P)

#Make the various data.frames!---------------------------------------------------------

#ABSOLUTE <- readWorksheet(Fp_wbk, "ABSOLUTE")
ABSOLUTE <- read.delim('data/pavela_flat_files/EDADABS.txt', header=T, sep="\t")

AGE <- readWorksheet(Fp_wbk,"AGE")

#AGENT <- readWorksheet(Fp_wbk,"AGENT")
AGENT <- read.delim('data/pavela_flat_files/AGENTE.txt', header=T, sep="\t")

COMMENT <- readWorksheet(Fp_wbk,"COMMENT")

#need the CULTURE worksheet to get the date associations for the cultural sites
# CULTURE <- readWorksheet(Fp_wbk,"CULTURE")
CULTURE <- read.delim('data/pavela_flat_files/CULTURAL.txt', header=T, sep="\t")

#DEPOSIT <- readWorksheet(Fp_wbk,"DEPOSIT")
DEPOSIT <- read.delim('data/pavela_flat_files/DEPOSITO.txt', header=T, sep="\t")

#FM_LOCALITY is the curated version of the LOCALITY table to be used for Neotoma
# FM_LOCALITY <- readWorksheet(Fp_wbk,"FM_LOCALITY")
FM_LOCALITY <- read.delim('data/pavela_flat_files/LOCALIDA.txt', header=T, sep="\t")

#FM_Synonyms is a reference to the Neotoma Synonyms table. I'll need a copy of this table to 
#link this data up. They are dropped in as synonym tags in the spreadsheet entry for the taxon name
FM_Synonyms <- readWorksheet(Fp_wbk,"FM_Synonyms")

#FM_Taxa is the curated data table to use for the Neotoma database
#FM_Taxa <- readWorksheet(Fp_wbk,"FM_Taxa")
FM_Taxa <- read.delim('data/pavela_flat_files/ESPECIES.txt', header=T, sep="\t")
  
#these tables are needed for building the contacts and the references

NEOTOMA_Contacts <- readWorksheet(Fp_wbk,"NEOTOMA_Contacts")

NEOTOMA_PublicationAuthors <- readWorksheet(Fp_wbk,"NEOTOMA_PublicationAuthors")
# AUTORPUB

NEOTOMA_PublicationEditors <- readWorksheet(Fp_wbk,"NEOTOMA_PublicationEditors")

NEOTOMA_Publications <- readWorksheet(Fp_wbk,"NEOTOMA_Publications")

#This Table links MachineNo with Neotoma PublicationID
PUB_FAUNA_REFER <- readWorksheet(Fp_wbk,"PUB_FAUNA_REFER")

#this table will be needed for ingesting the Pliocene sites with lithostrat named units
#I'm reading it in now to use for subsetting the FM_Locality data
STRATIGRAPHY <- readWorksheet(Fp_wbk,"STRATIGRAPHY")

#this lookup is used to get the repository names
L_REPOS <- readWorksheet(Fp_wbk,"L_REPOS")

#Dana's code for binding the synonmys on 
Synonymnames <- loadWorkbook('SynonymNames.xlsx')

Synnames <- readWorksheet(Synonymnames, "Sheet1")

names(Synnames)[names(Synnames)=="SynonymID"] <- "SynonymId"

synnames2 <- join(Synnames, FM_Synonyms, by= "SynonymId") 
FM_Taxa <- join(FM_Taxa, synnames2, by= "FaunalId")

#Are there any duplicated Age lines? there shouldn't be: one AgeModel for each AnalysisUnit
#AGE[duplicated(AGE[,c("Machine..","AnalysisUnit")]),]

#which pubs aren't in the NEOTOMA_Publications df?
#PUB_FAUNA_REFER <- PUB_FAUNA_REFER[!is.na(PUB_FAUNA_REFER$NeotomaID),]
#PUB_FAUNA_REFER[!(PUB_FAUNA_REFER$NeotomaID %in% NEOTOMA_Publications$NeotomaID),"NeotomaID"]

#which authors aren't in the NEOTOMA_Authors df?
#PUB_FAUNA_REFER[!(PUB_FAUNA_REFER$NeotomaID %in% NEOTOMA_PublicationAuthors$NeotomaID),"NeotomaID"] %>% unique

#which sites have no PUB_FAUNA_REFER values?
#FM_LOCALITY[!(FM_LOCALITY$Machine. %in% PUB_FAUNA_REFER$Machine.),"Machine."]

# Building the TLX file ------------------------------------------



#Need the list to XML function. Remember, you have to have an existing XML node object to drop in as
#the first argument in this function.

#this version adds attributes. Gosh I wish I'd figured this out ages ago.
#I've also made it so that "" and NA lead to empty nodes, not blank nodes.

listToXML <- function(node, sublist){
  for(i in 1:length(sublist)){
    
    if(names(sublist)[i]==".attrs"){
      addAttributes(node, .attrs = sublist[[i]])
    } else {
      
      child <- newXMLNode(names(sublist)[i], parent=node);
      
      if (typeof(sublist[[i]]) == "list"){
        listToXML(child, sublist[[i]])
      }
      else {
        if(sublist[[i]] != "" && !is.na(sublist[[i]])){
        xmlValue(child) <- sublist[[i]]
      }
      }
  }
  }}  

#Next step: Select out the information from each df by site, so that I can build a single TLX file for that site.

#The best way I can think of to do this is with a loop using the Site 

#In Faunmap, there are no "Collection Units", so everything loops by sites with multiple analysis units in each site.

#so, workflow is to:
    #1) grab the info for the site, and collection unit (there is only one CU and DS per site
      #in the Pampas data, but the UE data make use of the distinction between sites and collection 
      #units, so I'll have to build a loop for CU, too).
    #2) Grab the data for Publications
    #3) Grab the data for the Contacts from the Publications (may not be necessary)
    #4) Grab the data for the dataset, link to Publications, Contacts, fill with repository info from Repository df
    #5) Build the specimen block
      #Available fields include: Taxon, TaxaGroup, Element, Symmetry (left/right), Portion (partial
      #/whole?), Maturity, Sex, DomesticStatus, Preservative, NISP, Repository, SpecNr, FieldNr, 
      #ArctosNr, GenBankNr, Notes.
      
      #I think these are the bare minimum fields we'll get from Antigua: Taxon, TaxaGroup (mammals), 
      #Element, NISP, Repository, SPecNr, Notes
    
    #6) Build the Geochronology and Chronology blocks.
      #This should probably be two numbers, but I still don't quite see the difference in the two. 
      #I think the Geochronology is just the list of the dates on the site. Links to Specimen block
      #I think the Chronology uses the Geochronology data to build the "age model" which will be
      #nothing for these sites, because they are all single collection units within sites.
      

    #7) Build the spreadsheet of occurrences. 
      #This step will be complex, because multiple specimens of the same species will have to be 
      #gathered into single occurrences for the spreadsheet. Will also need to link the data from the 
      #chronology block to the proper rows in the spreadsheet.

#subset FM_locality--------------------------------------------

#removing sites that have stratigraphic data
#change here for exporting the Strat site data--------------

#old version that skips the strat site data
#FM_LOCALITY <- FM_LOCALITY[!(FM_LOCALITY$Machine. %in% STRATIGRAPHY$Machine.),]

#new version that only does the strat site data
FM_LOCALITY <- FM_LOCALITY[(FM_LOCALITY$Machine. %in% STRATIGRAPHY$Machine.),]


#removing sites that have synonymy data

#have to link from FM_Synonyms$FaunalID to MachineNo from FM_Taxa

#syn_mach <- join(FM_Synonyms, FM_Taxa)

#FM_LOCALITY <- FM_LOCALITY[(FM_LOCALITY$Machine. %in% syn_mach$Machine.),]

#this leaves us with 1323 of 2056 sites to build into Tilia files. That gets 64% of them done.

#outermost loop, for collection unit------------------------------------------------

for(MachineNumber in FM_LOCALITY$Machine.) {

  #testing with MachineNo...
  #MachineNumber <- "3014"



#Subset variables for this iteration------------------------------------------------

  #establish the SiteName for this MachineNumber

  SiteName <- FM_LOCALITY[MachineNumber == FM_LOCALITY$Machine.,"SiteName"]

  #Also need to create a handle for each of these sites
  Handle <- paste0("FM",MachineNumber)

  
  #Subset the other df to get just the values for the current CU MachineNumber
  ABSOLUTE_sub <- ABSOLUTE[ABSOLUTE$Machine. == MachineNumber,]
  #need to have the row names reset for ABSOLUTE_sub to facilitate links in AgeModel node
  rownames(ABSOLUTE_sub) <- seq(length=nrow(ABSOLUTE_sub))
  AGE_sub <- AGE[AGE$Machine.. == MachineNumber,]
  AGENT_sub <- AGENT[AGENT$Machine. == MachineNumber,]
  COMMENT_sub <- COMMENT[COMMENT$Machine. == MachineNumber,]
  DEPOSIT_sub <- DEPOSIT[DEPOSIT$Machine. == MachineNumber,]
  FAUNAL_sub <- FM_Taxa[FM_Taxa$Machine. == MachineNumber,]
  Pub_sub <- PUB_FAUNA_REFER[PUB_FAUNA_REFER$Machine. == MachineNumber,]
  #ditch publications with no NeotomaID
  Pub_sub <- Pub_sub[!is.na(Pub_sub$NeotomaID),]
  
#Build the top level XML node------------------------------------------------

  XML_out <- newXMLNode("TiliaFile")
  
  #have to adjust Tilia version here if needed
  
  version_list <- list("Tilia", 2, 0, 41)
  names(version_list) <- c("Application", "MajorVersion", "MinorVersion", "Release")
  version <- list(version_list)
  names(version) <- "Version"
  listToXML(XML_out, version)
 

#Build the Site XML Block -----------------------------------------------------
  
  #What goes into a Site block in the TLX?
  #SiteName, LongEast, LongWest, LatNorth, LatSouth, Altitude, Country, State, County, SiteDescription
  #note that LongEast/West and LatNorth/South can be the same values if no bounding box.
  
  #need to make a list of XML nodes from the values of site on the row where SiteName is.

  SiteList <- as.list(FM_LOCALITY[FM_LOCALITY$Machine. == MachineNumber,])
  
  #new node to fill with listToXML()
  SiteNode<-newXMLNode("Site")

  #need to remove all NA's from SiteList here.
  SiteList <- SiteList[!is.na(SiteList)]
  
  #need to remove non-Neotoma fields
  SiteList <- SiteList[!(names(SiteList) %in% c("ID","Machine.", "DatasetNotes", "upsize_ts", "Repository"))]
  
  #then zip it to XML
  listToXML(SiteNode, SiteList)
  
  #append SiteNode to XML_out
  addChildren(XML_out, kids = list(SiteNode))

#Build the Contacts XML Block -----------------------------
  if(nrow(Pub_sub) > 0){
  #Contacts for FAUNMAP data includes only information from Pub_sub
  #Contacts will be only Authors
  #First must extract all Authors from Pub_sub

  authors <- join(Pub_sub, NEOTOMA_PublicationAuthors)
  
  authors <- authors["NeotomaContactID"]
  
  authors <- join(authors, NEOTOMA_Contacts)
  
  #similarly for editors

  editors <- join(Pub_sub, NEOTOMA_PublicationEditors)
  
  #remove any lines for publicaitons with no editors
  editors <- editors[!is.na(editors$FamilyName),]

  #note that editors will have no information unless the publication has editors... Also
  #the editors aren't in the NEOTOMA_Contacts table, apparently.
  #they'll still have to be pulled in for the contacts.
  
  #Contacts can have almost all of the fields from authors, so have to pare a few
  #Most of the authors added for FAUNMAP2 don't have suffix, title, telephone, email, address, etc.
  
  contacts_df <- authors[,c("NeotomaContactID", "NeotomaAliasID", "FullContactName","FamilyName", "LeadingInitials", "GivenNames")]
  
  contacts_df <- merge(contacts_df, editors[,c("FamilyName", "LeadingInitials")], all = TRUE)

  #contacts is now a df, but it needs to be a list with each df element nested as a list item
  #also, each list item needs an ID #
  
  contacts <- list()
  
  
  for(i in 1:nrow(contacts_df)){
    #has to only pull filled fields b/c editors have incomplete info
    contacts[[i]] <- as.list(contacts_df[i,!is.na(contacts_df[i,])])
    
    contacts[[i]]$.attrs <- c(ID = i)
    
  }

  #now we're back in line with the old script!
  
  names(contacts) <- c(rep('Contact',times = length(contacts)))
  
  #make top level Contacts node
  ContactsNode <- newXMLNode("Contacts")

  #make the xml for contacts
  contacts_xml <- listToXML(ContactsNode, contacts)  
  
  #add Contacts to output
  addChildren(XML_out, kids = list(ContactsNode), append = TRUE)
  
  #now I have to use the contacts object to check the ID # of contacts to link them in on the Authors block.
  
  }#end of contacts if statement for no Pubs
#Build the Collection Unit XML Block ----------------

  
  #What goes into a Collection Unit block in the TLX?
  #CollectionUnit(Handle, CollectionType, CollectionDevice, Collectors (Contact ID="?"), Collection Date, Depositional Environment, Notes)
  
  #Looks like none of these are necessary except the Handle. 
  #The Date may be in days since 1960 or something similar.
  
  #DepositionalEnvironment has been fixed to the controlled vocab for Tilia

  CUList <- list()
  
  #add the elements we need
  CUList$Handle <- Handle
  CUList$CollectionType <- "Isolated Specimen"
  if(nrow(DEPOSIT_sub)>0){
  CUList$CollectionDevice <- unique(DEPOSIT_sub$CollectionDevice)
  CUList$DepositionalEnvironment <- unique(DEPOSIT_sub$DepositionalEnvironment)
  }
  #Remove NA's
  CUList <- CUList[!is.na(CUList)]
  
  #new node to fill with listToXML()
  CUNode<-newXMLNode("CollectionUnit")
  
  listToXML(CUNode, CUList)
  
  #append SiteNode to XML_out
  addChildren(XML_out, kids = list(CUNode))

  
#Build the Publications XML Block -------------------------------------------------------

    
  #What goes into a Publication block in the TLX?
  #Publications (Publication ID="1" (PublicationType, NeotomaID, DOI, PublicationYear, Citation, Authors (Author (Contact ID="1", LastName, Initials)), Title, Journal, Volume, Issue, Pages))
  
  #for these well-formatted publications, I can use an approach like I did for authors
  
  if(nrow(Pub_sub) > 0){
  
  Pubs_list <- list()
 
  i <- 1
  for(PUB_ID in Pub_sub$NeotomaID){
    #PUB_ID <- 11155
    
    Publication <- as.list(NEOTOMA_Publications[NEOTOMA_Publications$NeotomaID == PUB_ID,])
    
    #remove empty list elements
    Publication <- Publication[!is.na(Publication)]
    
    authors_temp <- NEOTOMA_PublicationAuthors[NEOTOMA_PublicationAuthors$NeotomaID == PUB_ID,"NeotomaContactID"]
    authors_temp <- NEOTOMA_Contacts[NEOTOMA_Contacts$NeotomaContactID %in% authors_temp,]
    authors_temp <- authors_temp[,c("FamilyName", "LeadingInitials")]
    names(authors_temp) <- c("LastName", "Initials")
    for(j in 1:nrow(authors_temp)){
    
    Author <- as.list(authors_temp[j,])
    Publication$Authors[[j]] <- Author
    names(Publication$Authors)[j] <- "Author"
    }
    
    editors_temp <- NEOTOMA_PublicationEditors[NEOTOMA_PublicationEditors$NeotomaID == PUB_ID,c("FamilyName", "LeadingInitials")]
    names(editors_temp) <- c("LastName", "Initials")
    
    if(nrow(editors_temp)>0){
    for(j in 1:nrow(editors_temp)){
      
      Editor <- as.list(editors_temp[j,])
      Publication$Editors[[j]] <- Editor
      names(Publication$Editors)[j] <- "Editor"
    }
    }
    
    
    Publication$.attrs <- c(ID = i, Primary = "false")
    if(i==1){Publication$.attrs <- c(ID = i, Primary = "true")}

    Pubs_list[[i]] <- Publication
    names(Pubs_list)[i] <- "Publication"
    
    
    
    i <- i + 1
    
  }


  
  #first, make Publications node
  Publications<-newXMLNode("Publications")

  
  #make sure there _are_ publications for the site.
  if(length(Pubs_list)>0){
    
    listToXML(Publications, Pubs_list)
    
    
    

  #Add Publications to top XML node
  
  addChildren(XML_out, kids = list(Publications), append = TRUE)
  
  } #end of if statement for whether there are pubs: no pubs, no Publicaitons block in XML
  
  #Note: if there are no publications, need to keep publication list out of Dataset block
  
  
  #2) make a way to track Publication ID's. Solution: save the Pubs_list object. It has the abbreviation (key field) as well as the index order of the pubs for the TLX file.
  
  
  #also: rather than trying to deal with all of the contacts, I am going to try dropping the Author blocks into Tilia with no Conctact ID and making the creation of a contact part of the data cleaning.
  #This works: Tilia links the contacts for each of the authors in the Publications block to the contacts block automatically!
  #However, I have gone ahead and built the code to index the authors, saving mouse clicks down the road.
  
  #none of this is necessary for the FAUNMAP II dataset because it only has contacts in the 
  #publications block, so I've dropped all of the contact number tracking from this version.
  
}#end of if statement for no pubs

#Build the Dataset XML Block -------------------------------------------------------------

  #What goes into a Dataset block in the TLX?
  #First, there can be multiple Datasets, one for each DatasetType. 
  #In this case DatasetType will be "Vertebrate fauna" each time.
  
  
  #Dataset(DatasetType, Notes, IsSSamp (T/F), IsAggregate (T/F), Investigators (Contact ID = "?"), Processors (Contact ID = "?"), Publications (Publication ID = "?"))

  #Will not have investigators or processors in the FAUNMAP II data. 
  #Publicaitons will be the same as main publication block...
    
  #Begin building the dataset list for XML conversion, adding the default fields, plus Notes
  Dataset_list <- list(DatasetType = 'Vertebrate fauna',
                       IsSSamp = 'False',
                       IsAggregate = 'False',
                       Notes = FM_LOCALITY[FM_LOCALITY$Machine. == MachineNumber,"DatasetNotes"])
  
  #Investigators gathered for contacts generation

  Dataset_XML<-newXMLNode("Dataset")
  
  listToXML(Dataset_XML, Dataset_list)

  #now add the pubs
  
  #this wasn't working because I had length(Pub_sub) instead of nrow(Pub_sub)
  if(nrow(Pub_sub)>0){
  DS_Publications <- newXMLNode("Publications")
  
  for(i in 1:length(xmlToList(Publications))){
    newXMLNode("Publication", attrs = c(ID = i), parent = DS_Publications)
  }
  
  addChildren(Dataset_XML, kids = list(DS_Publications), append = TRUE)
  } #end of if statement that skips adding pubs if there are no pubs
  

  
  #Build Repository Block in Dataset Block-------------------

  #repository data are in the FM_LOCALITY$Repository column as the acronym: 
  #need to pull full name with a dictionary lookup 
  #Some have two repositories in a comma delimited list
  
  Repository_sub <- FM_LOCALITY[FM_LOCALITY$Machine. == MachineNumber,"Repository"]
  
  if(!is.na(Repository_sub)){
    
    #first, pull apart if there are a comma-delimted set
    Rep_df <- as.data.frame(strsplit(Repository_sub, ", ", fixed = TRUE))      
    names(Rep_df) <- "Acronym"
    
    #lookup the Repository name from the dictionary
    for(Acronym in Rep_df$Acronym){
      Rep_df[Rep_df$Acronym == Acronym, "Repository"] <- L_REPOS[L_REPOS$Acronym == Acronym, "Repository"]
      
    }
    
    #need to convert Rep_df to a list where each row is a "Repository"
  Rep_list <- list()
  
  for(r in 1:nrow(Rep_df)){
   
    Rep_list[[r]] <- as.list(Rep_df[r,]) 
    
  }
    
  #name each entry in Rep_list "Repository" (as if it weren't confusing enough)
  names(Rep_list) <- c(rep('Repository',times = length(Rep_list)))
  
  
    #build the output Rep_list
    
    Repositories_XML<-newXMLNode("Repositories")
    
    listToXML(Repositories_XML, Rep_list)
    
    addChildren(Dataset_XML, kids = list(Repositories_XML), append = TRUE)
    
  }
  

  Dataset_top<-newXMLNode("Datasets", .children = Dataset_XML)
  
  addChildren(XML_out, kids = list(Dataset_top), append = TRUE)
  

#Build the Geochronology and Chronology blocks----------------------


#The Geochronology is just the list of the dates on the site. Links to Specimen block.
#The Chronology uses the Geochronology data to build the "AgeModel"
#This has to be constructed in Tilia after the data are ingested.

 
#Build the Geochronology XML Block------------------------------------------------------------------

#Nodes: GeochronDataset(Investigators(Contact ID="1"), Geochronology AnalysisUnitID="Name" (GeochronSample ID="1" (Method, AgeUnits, AnalysisUnit, LabNumber, Age, ErrorOlder, ErrorYounger, Sigma, StdDev, GreaterThan (value: FALSE), Parameters (Parameter (Name, Value), Parameter (Name, Value)...) Material Dated, PublicationsText (value: abbr cite), Publications (Publication ID="3"), TaxaDated (TaxonDated (ID, SpecimenID, SpecNr, Taxon, Element, Fraction, CalAgeMedian, CalAgeOlder, CalAgeYounger, CalibCurve, CalibProgram (Program, Version)))), GeochronSample ID="2"...))
  

#so, each observation in the ABSOLUTE df gets a GeochronSample entry
 
#loop through ABSOLUTE creating GeochronSamples. If we have to add Analysis Units, no sweat: they are tracked as an attribute of the GeochronSample, so just pull it in there! The <Geochronology AnalysisUnitID="Name"> is a constant, even with multiple analysis units.

  #skip the rest of this if there are no ABSOLUTE_sub values
  if(nrow(ABSOLUTE_sub)>0){

  #make the node for GeochronDataset
  GeochronDataset <- newXMLNode("GeochronDataset")

    
  #make the node with the attribute
  Geochronology<-newXMLNode("Geochronology", attrs = c(AnalysisUnitID = "Name"))

for(i in 1:nrow(ABSOLUTE_sub)){

  #make a list with the values for the geochron samples in this analysis unit
  
  param_list <- list()
  
  #list of filled parameters for Carbon-14 dates
    if(ABSOLUTE_sub[i,"GeochronType"] == "Carbon-14"){
    param_list[[length(param_list)+1]] <- list(Name = "Method", Value = if(ABSOLUTE_sub[i,"DatingMethod"] %in% c("14CA","XADC","XAD","XAGE")){"accelerator mass spectrometry"} else if(ABSOLUTE_sub[i,"DatingMethod"] %in% c("14CG","14CS","14CU")){"conventional radiometric"}else{"unspecified"})
    names(param_list)[1:length(param_list)] <- "Parameter"
    }


  
    #have to comment this out b/c there are no pubs associated with dates in FMII data structure
    # pub_ID <- list()
    # #find the publication associated with this date
    # #it's extra complicated because some have two pubs.
    # GC_spec_pub <- as.list(strsplit(ABSOLUTE_sub[i,"Citation"], "|", fixed = TRUE)[[1]])
    # if(length(Pubs_list)>0){
    # for(p in 1:length(Pubs_list)){
    #   if(Pubs_list[[p]]$Abbr %in% GC_spec_pub){
    #   pub_ID[[length(pub_ID)+1]]<-p}
    # }}
    # 

  
  #need to makethis data pull from the updated ABSOLUTE table. need to include other flags 
  #from the lookup table in the archive excel table.
    tmp_list <- list(Method= ABSOLUTE_sub[i,"GeochronType"],
                  AgeUnits = "Radiocarbon years BP",
                  AnalysisUnit = ABSOLUTE_sub[i,"Analysisunit"],
                  LabNumber = if(!is.na(ABSOLUTE_sub[i,"Lab"])){ABSOLUTE_sub[i,"Lab"]}else{"missing"},
                  Age = ABSOLUTE_sub[i,"AssociatedDate"],
                  ErrorOlder = ABSOLUTE_sub[i,"StandardDeviation"],
                  ErrorYounger= ABSOLUTE_sub[i,"StandardDeviation"],
                  Sigma = "1",
                  StdDev= ABSOLUTE_sub[i,"StandardDeviation"],
                  GreaterThan= "False",
                  Parameters = if(length(param_list)>0){param_list}else{NA},
                  MaterialDated= ABSOLUTE_sub[i,"MaterialType"],
                  Notes = ABSOLUTE_sub[i,"Notes"]
                  #no publication data for dates in FMII
                  #PublicationsText= ABSOLUTE_sub[i,"Citation"]
                  )
      #have to add TaxaDated if there is a named taxon
      #doesn't have to tie to taxon list!
        if(!is.na(ABSOLUTE_sub[i,"Taxon"])){
            tmp_list$TaxaDated <- list(TaxonDated = list(
                      ID= i, #as far as I can tell, this is the same as the GeochronSample ID

                      #Note: this Specimen ID is the ID="#" from the specimen block
                      #or a -1 if there is no associated specimen, as in FMII
                      SpecimenID = -1,
                      Taxon= ABSOLUTE_sub[i,"Taxon"],
                      #don't have Elements in FMII
                      #Element= ABSOLUTE_sub[i,"Element"],
                      #add Fraction if known
                      Fraction= if(!is.na(ABSOLUTE_sub[i,"Fraction"]))
                                {ABSOLUTE_sub[i,"Fraction"]}
                                else{"Unknown"}
                      ))
        } #end of the TaxaDated if statement

    #no publication data in FMII, so this block not needed
    #have to add publication block here, because empty is no good.
    # Pub_geochron <- list()
    # if(length(pub_ID)>0){
    #   for(foo in 1:length(pub_ID)){
    #   Pub_geochron[[length(Pub_geochron)+1]] <- list(.attrs = c(ID = pub_ID[[foo]]))
    #   names(Pub_geochron)[length(Pub_geochron)] <- "Publication"
    #   }
    #   tmp_list[[length(tmp_list)+1]]<- Pub_geochron
    #   names(tmp_list)[length(tmp_list)]<-"Publications"
    # 
    #   }


    #make geochron sample XML node, with ID tag

    GeochronSample<-newXMLNode("GeochronSample", attrs = c(ID = i))

    listToXML(GeochronSample, tmp_list)

    addChildren(Geochronology, kids = list(GeochronSample), append = TRUE)

  } #end of GeochronSample / ABSOLUTE_sub loop


  #need to drop this list in place of the current collector node.
  #Not sure if Tilia will care if the rest of the contact info is there...
  #Tilia does not care about the rest of the contact info.

  addChildren(GeochronDataset, kids = list(Geochronology), append = TRUE)
  addChildren(XML_out, kids = list(GeochronDataset), append = TRUE)

  #merge them together...
  }#end of if(nrow(ABSOLUTE_sub)>0)

  
#Build the AgeModels block----------------------
  
  #Fields Tilia wants inside an AgeModel: ChronNumber(1), ChronologyName (Faunmap 2.1), AgeUnits(Calibrated radiocarbon years BP), Default (True), Model(??), AgeBoundOlder, AgeBoundYounger, Notes, ChronControls( ChronControl( Control Type(from controlled vocab), AnalysisUnit(from AU list), Age, AgeLimitOlder, AgeLimitYounger, CalibCurve, CalibProgram(Program, Version), GeochronLinks( GeochronLink ID="3"[ties to geochronID])))  

  
  #I will have to build a switch that runs this one way if there are dates in the ABSOLUTE_sub table
  #and another if there are not.
  
  #make the node for AgeModels with the attribute
  AgeModels<-newXMLNode("AgeModels", attrs = c(AnalysisUnitID = "Name"))
  #There is actually only one AgeModel for each site
  AgeModel_list<- list(ChronNumber = 1,
                  ChronologyName = "Faunmap 2.1",
                  AgeUnits = AGE_sub$AgeUnits,
                  Default = "True",
                  AgeBoundOlder = max(AGE_sub$MaximumAge),
                  AgeBoundYounger = min(AGE_sub$MinimumAge)
              )
  
  AgeModel<-newXMLNode("AgeModel")
  
  listToXML(AgeModel, AgeModel_list)
  
  ChronControls<-newXMLNode("ChronControls")
  
  #each AnalysisUnit gets a ChronControl, and if there are multiple dates for that AU,
  #they get multiple links
  for(control_count in 1:nrow(AGE_sub)){
    

    #build a list with the values for that from the MinAgeControl Column
    #make the control have the ages for the analysis unit
    control_list <- list(ControlType = AGE_sub[control_count,"ControlTypeMinAge"], 
                         AnalysisUnit = AGE_sub[control_count,"AnalysisUnit"], 
                         AgeLimitYounger = AGE_sub[control_count,"MinimumAge"],
                         AgeLimitOlder = if(is.na(AGE_sub[control_count,"ControlTypeMaxAge"])){AGE_sub[control_count,"MaximumAge"]}else{NA},
                         Notes = AGE_sub[control_count,"Notes"]
                         )
                      
    #create the ChronControl XML node
    
    ChronControl<-newXMLNode("ChronControl")
    
    listToXML(ChronControl, control_list)
    
    #make links if present
    #need a switch here in case the lower bound is not a geochron
    if(AGE_sub[control_count,"ControlTypeMinAge"] %in% c("Radiocarbon", "Sangamon", "Gilbert", "Gauss","Matuyama","Potassium-argon","Uranium-series","Argon-argon","C1n (Brunhes)","Geomagnetic polarity time scale","Fission track","C2n (Olduvai)")){  
  
    GeochronLinks<-newXMLNode("GeochronLinks")
    
    #let's subset ABSOLUTE_sub by the AnalysisUnit in this loop
    link_df <- ABSOLUTE_sub[ABSOLUTE_sub$Analysisunit == AGE_sub[control_count,"AnalysisUnit"],]
    
    #Tilia wants the ID numbers of the radiocarbon dates in this link.
    #so I reset the row.names(ABSOLUTE_sub) so that they are carried in here
    
    for(link_ID in row.names(link_df)){
      
      GeochronLink<-newXMLNode("GeochronLink", attrs = c(ID = link_ID))
      
      addChildren(GeochronLinks, kids = list(GeochronLink), append = TRUE)
    }#end of link_ID loop
    
    addChildren(ChronControl, kids = list(GeochronLinks), append = TRUE)
    }#end of if() related to geochron 
    
    addChildren(ChronControls, kids = list(ChronControl), append = TRUE)
    
    #if there is a MaxAgeControl, build a control_list for that one, too... 
    if(!is.na(AGE_sub[control_count,"ControlTypeMaxAge"])){
      
      control_list <- list(ControlType = AGE_sub[control_count,"ControlTypeMaxAge"], 
                           AnalysisUnit = AGE_sub[control_count,"AnalysisUnit"],
                           AgeLimitOlder = AGE_sub[control_count,"MaximumAge"],
                           Notes = AGE_sub[control_count,"Notes"]
      )
      
      #create the ChronControl XML node
      
      ChronControl<-newXMLNode("ChronControl")
      
      listToXML(ChronControl, control_list)
      
      #make links if present
      #need a switch here in case the lower bound is not a geochron
      if(AGE_sub[control_count,"ControlTypeMinAge"] %in% c("Radiocarbon", "Sangamon", "Gilbert", "Gauss","Matuyama","Potassium-argon","Uranium-series","Argon-argon","C1n (Brunhes)","Geomagnetic polarity time scale","Fission track","C2n (Olduvai)")){  
        
        GeochronLinks<-newXMLNode("GeochronLinks")
        
        #let's subset ABSOLUTE_sub by the AnalysisUnit in this loop
        link_df <- ABSOLUTE_sub[ABSOLUTE_sub$Analysisunit == AGE_sub[control_count,"AnalysisUnit"],]
        
        #Tilia wants the ID numbers of the radiocarbon dates in this link.
        #so I reset the row.names(ABSOLUTE_sub) so that they are carried in here
        
        for(link_ID in row.names(link_df)){
          
          GeochronLink<-newXMLNode("GeochronLink", attrs = c(ID = link_ID))
          
          addChildren(GeochronLinks, kids = list(GeochronLink), append = TRUE)
        }#end of link_ID loop
        
        addChildren(ChronControl, kids = list(GeochronLinks), append = TRUE)
      }#end of if() related to geochron 
      
      addChildren(ChronControls, kids = list(ChronControl), append = TRUE)
      
    } 
    
    
    
  } #end of loop for age_au
  
  #add the ChronControls to the AgeModel
  addChildren(AgeModel, kids = list(ChronControls), append = TRUE)
  #then add the AgeModel to AgeModels
  addChildren(AgeModels, kids = list(AgeModel), append = TRUE)
  
  #lastly, add the AgeModels to the overall Tilia XML
  addChildren(XML_out, kids = list(AgeModels), append = TRUE)
  
  
#Build the spreadsheet of occurrences---------------------------------------------------------

#thank Babbage that the FAUNMAP data have no specimens to deal with!

#Tilia won't read without the SpreadSheetOptions!
  
  #Make the SpreadsheetBook XML node
  
  SpreadSheetBook <- newXMLNode("SpreadSheetBook")
  
  #make the SpreadSheetOptions XML node
  SpreadSheetOptions <- newXMLNode("SpreadSheetOptions")
  
  #Make the GroupCategories list
  GroupCategories <- list("Mammals")
  names(GroupCategories) <- "GroupCategory"
  #Make the list of values that go into the SpreadSheetOptions
  SHO_list <- list(0, "Arial", 9, 64, 18, 1, "True", "False", "True", "True", "True", "False", "True", "True", GroupCategories)
  names(SHO_list) <- c("HeaderRow", "FontName", "FontSize", "DefaultColWidth", "DefaultRowHeight", "PercentDecimalPlaces", "CheckDupCodes", "CaseSensitiveCodes", "CodesVisible", "ElementsVisible", "UnitsVisible", "ContextsVisible", "TaphonomyVisible", "GroupsVisible", "GroupCat")
  listToXML(SpreadSheetOptions, SHO_list)
  
  addChildren(SpreadSheetBook, kids = list(SpreadSheetOptions))
  
  #build the Spreadsheet block
  #unless there are no specimens, in which case, pfft!
  #taxa nodes?--------
  if(nrow(FAUNAL_sub) > 0){
  
  #first, build the df that has the shape of the required first columns of the spreadsheet. Note: it should have two more rows than the count of distinct_taxa, for the headers.
    #and if there are both MNI and NISP, it will have the list twice, but if it's just P/A it will be once and similarly if there is only MNI or NISP. Le sigh.
  #so: go through and make three spreadsheet taxon blocks: 
  #one for presence/absence, 
  #one for MNI, 
  #and one for NISP.
  #Only populate the MNI and NISP blocks if there are values there.
  #then you will always have a presence/absence block, but you will also get the MNI and NISP if the data are there.
  #if there are distinct Modifications for the same taon, they have to be on different rows!

  distinct_taxa <- FAUNAL_sub %>% select(TaxonName, Modification, InvalidTaxonName) %>% distinct
  
  #Add the agemodel rows (older/younger) here-------------
  
  spreadsheet<- data.frame(Code=c("", "", "#Chron1.Old", "#Chron1.Young", rep("", nrow(distinct_taxa))),
                           Name= c("","", "Age (Chron1 Older Bound)", "Age (Chron1 Younger Bound)", distinct_taxa$TaxonName), 
                           Element= c("","","","",rep("bone", nrow(distinct_taxa))), 
                           Units=c("","","","",rep("present/absent", nrow(distinct_taxa))), 
                           Context = c(rep("", nrow(distinct_taxa)+4)), 
                           Taphonomy = c("", "", "", "", distinct_taxa$Modification), 
                           Group=c(rep("", nrow(distinct_taxa)+4))
                           )
  
  distinct_au <- FAUNAL_sub %>% select(AnalysisUnit) %>% distinct
  
  
  #add columns for each analysis unit, put in presence/absence
  
  for(i in 1:nrow(distinct_au)){
    spreadsheet[length(spreadsheet)+1] <- c("", 
                                            distinct_au[i,"AnalysisUnit"], 
                                            AGE_sub[AGE_sub$AnalysisUnit == distinct_au[i,"AnalysisUnit"],"MaximumAge"], 
                                            AGE_sub[AGE_sub$AnalysisUnit == distinct_au[i,"AnalysisUnit"],"MinimumAge"], 
                                            rep("", nrow(distinct_taxa))) 
   
     for(j in 1:nrow(distinct_taxa)){
      #within this row, look through the whole FAUNAL_sub df for a match
      for(k in 1:nrow((FAUNAL_sub))){
        
        #if taxon and analysis unit match, give Present
        if(spreadsheet[j+4,"Name"] == FAUNAL_sub[k,"TaxonName"] &&
           spreadsheet[2,length(spreadsheet)] == FAUNAL_sub[k,"AnalysisUnit"] &&
           spreadsheet[j+4,"Taphonomy"] %in% FAUNAL_sub[k,"Modification"]

           #can skip all this code because %in% does the matching of NAs!!!!
           # (if(!is.na(spreadsheet[j+4,"Taphonomy"]) && !is.na(FAUNAL_sub[k,"Modification"])){
           #   spreadsheet[j+4,"Taphonomy"] == FAUNAL_sub[k,"Modification"]
           # }else{
           #   if(is.na(spreadsheet[j+4,"Taphonomy"]) && is.na(FAUNAL_sub[k,"Modification"])){TRUE}
           #   else{FALSE}
           # })
           ) {
          
          spreadsheet[j+4,length(spreadsheet)] <- 1
        } #end of if statement
        
      } #end of k loop through taxon list of FAUNAL_sub
      
    } #end of j loop through taxon list of spreadsheet
    
    
  } #end of analysis unit loop
  
  #make the MNI rows
  distinct_taxa_MNI <- FAUNAL_sub %>% select(TaxonName, Modification, MNI) %>% distinct
  
  distinct_taxa_MNI <- distinct_taxa_MNI[!is.na(distinct_taxa_MNI$MNI),]  
  
  if(nrow(distinct_taxa_MNI)>0){
    
  #remove multiple taxa if MNI is different in different AnalysisUnits
  distinct_taxa_MNI <-  distinct_taxa_MNI %>% select(TaxonName, Modification) %>% distinct
  
  #note: using the distinct_taxa_MNI because some taxa may have P/A and not MNI
  spreadsheet_MNI<- data.frame(Code=c(rep("", nrow(distinct_taxa_MNI)+1)),
                           Name= c("", distinct_taxa_MNI$TaxonName), 
                           Element= c("", rep("bone", nrow(distinct_taxa_MNI))), 
                           Units=c("", rep("MNI", nrow(distinct_taxa_MNI))), 
                           Context = c(rep("", nrow(distinct_taxa_MNI)+1)), 
                           Taphonomy = c("", distinct_taxa_MNI$Modification), 
                           Group=c(rep("", nrow(distinct_taxa_MNI)+1))
  )
  
  for(i in 1:nrow(distinct_au)){
    #this dims out the column for this AU
    spreadsheet_MNI[length(spreadsheet_MNI)+1] <- c(distinct_au[i,"AnalysisUnit"], rep("", nrow(distinct_taxa_MNI))) 
    
    #now to add correct MNI, walking down taxon list
    
    for(j in 1:nrow(distinct_taxa_MNI)){
      #within this row, look through the whole FAUNAL_sub df for a match
      for(k in 1:nrow((FAUNAL_sub))){
        
        #if taxon and analysis unit match, give NISP
        if(spreadsheet_MNI[j+1,"Name"] == FAUNAL_sub[k,"TaxonName"] &&
           spreadsheet_MNI[1,length(spreadsheet_MNI)] == FAUNAL_sub[k,"AnalysisUnit"] &&
           spreadsheet_MNI[j+1,"Taphonomy"] %in% FAUNAL_sub[k,"Modification"]
           # (if(!is.na(spreadsheet_MNI[j+1,"Taphonomy"]) && !is.na(FAUNAL_sub[k,"Modification"])){
           #   spreadsheet_MNI[j+1,"Taphonomy"] == FAUNAL_sub[k,"Modification"]
           # }else{
           #   if(is.na(spreadsheet_MNI[j+1,"Taphonomy"]) && is.na(FAUNAL_sub[k,"Modification"])){TRUE}
           #   else{FALSE}
           # })
           ) {
          
          spreadsheet_MNI[j+1,length(spreadsheet_MNI)] <- FAUNAL_sub[k,"MNI"]
        } #end of if statement
        
      } #end of k loop through taxon list of FAUNAL_sub
      
    } #end of j loop through taxon list of spreadsheet
    
  } #end of analysis unit loop
  
  #add spreadsheet_MNI to spreadsheet
  spreadsheet <- rbind(spreadsheet, spreadsheet_MNI[-1,])
  
  
  } #end of MNI if-statement
  
  #do the same thing for NISP
  
  #make the NISP rows
  distinct_taxa_NISP <- FAUNAL_sub %>% select(TaxonName, Modification, NISP) %>% distinct
  
  distinct_taxa_NISP <- distinct_taxa_NISP[!is.na(distinct_taxa_NISP$NISP),]  
  
  if(nrow(distinct_taxa_NISP)>0){
    
    #remove multiple taxa if NISP is different in different AnalysisUnits
    distinct_taxa_NISP <-  distinct_taxa_NISP %>% select(TaxonName, Modification) %>% distinct
    
    #note: using the distinct_taxa_NISP because some taxa may have P/A and not NISP
    spreadsheet_NISP<- data.frame(Code=c(rep("", nrow(distinct_taxa_NISP)+1)),
                                 Name= c("", distinct_taxa_NISP$TaxonName), 
                                 Element= c("", rep("bone", nrow(distinct_taxa_NISP))), 
                                 Units=c("", rep("NISP", nrow(distinct_taxa_NISP))), 
                                 Context = c(rep("", nrow(distinct_taxa_NISP)+1)), 
                                 Taphonomy = c("", distinct_taxa_NISP$Modification), 
                                 Group=c(rep("", nrow(distinct_taxa_NISP)+1))
    )
    
    for(i in 1:nrow(distinct_au)){
      #this dims out the column for this AU
      spreadsheet_NISP[length(spreadsheet_NISP)+1] <- c(distinct_au[i,"AnalysisUnit"], rep("", nrow(distinct_taxa_NISP))) 
      
      #now to add correct NISP, walking down taxon list
      
      for(j in 1:nrow(distinct_taxa_NISP)){
        #within this row, look through the whole FAUNAL_sub df for a match
        for(k in 1:nrow((FAUNAL_sub))){
          
          #if taxon and analysis unit match, give NISP
          if(spreadsheet_NISP[j+1,"Name"] == FAUNAL_sub[k,"TaxonName"] &&
             spreadsheet_NISP[1,length(spreadsheet_NISP)] == FAUNAL_sub[k,"AnalysisUnit"] &&
             spreadsheet_NISP[j+1,"Taphonomy"] %in% FAUNAL_sub[k,"Modification"]
             # (if(!is.na(spreadsheet_NISP[j+1,"Taphonomy"]) && !is.na(FAUNAL_sub[k,"Modification"])){
             #   spreadsheet_NISP[j+1,"Taphonomy"] == FAUNAL_sub[k,"Modification"]
             # }else{
             #   if(is.na(spreadsheet_NISP[j+1,"Taphonomy"]) && is.na(FAUNAL_sub[k,"Modification"])){TRUE}
             #   else{FALSE}
             # })
             ) {
            
            spreadsheet_NISP[j+1,length(spreadsheet_NISP)] <- FAUNAL_sub[k,"NISP"]
          } #end of if statement
          
        } #end of k loop through taxon list of FAUNAL_sub
        
      } #end of j loop through taxon list of spreadsheet
      
    } #end of analysis unit loop
    
    #add spreadsheet_NISP to spreadsheet
    spreadsheet <- rbind(spreadsheet, spreadsheet_NISP[-1,])
    
  } #end of NISP if-statement
  
  
  #note: no capital c for vectors, please.
  Spreadsheet_XML<-newXMLNode("spreadsheet", attrs = c(page = 0, name = "Data"))
  #make a blank list for the columns to hold the xml objects as they are created
  collist<-list()
  
  #cycle through each column in the spreadsheet
  for(i in 1:ncol(spreadsheet)){
    
    #make a blank list for the rows to hold the xml objects as they are created
    rowlist<-list()
    
    #make a node for this column and ID it as this ith col
    #note Col with a capital "C"
    col<-newXMLNode("Col", attrs = c(ID = i))
    
    
    
    #cycle through the rows for the ith column
    for(j in 1:nrow(spreadsheet)){
      #skip row if there is no text in this cell
      if(spreadsheet[j,i]=="" || is.na(spreadsheet[j,i])) next
      
      #making the jth row xml object and filling it with the value from the spreadsheet dataframe
      #if these are in the "NISP Zone", where i>7 and j>2, the node must be named "value"
      if(i>7 && j>2){row<-newXMLNode("cell", attrs = c(row = j), .children = list(newXMLNode("value", text = spreadsheet[j,i])))
      } else {
      row<-newXMLNode("cell", attrs = c(row = j), .children = list(newXMLNode("text", text = spreadsheet[j,i])))
      } #end of making row entry with "text" or "value"
      
      #here's where to add the synonymies, Danabot-------------------------
      #when you're in the taxon zone
      if(i == 2 && j>4){
        #check that synonym isn't NA
        if(!is.na(subset(distinct_taxa, TaxonName %in% spreadsheet[j,2] & Modification %in% spreadsheet[j,6])[,"InvalidTaxonName"])){
        synon_temp <- list(TaxonName = distinct_taxa[distinct_taxa$TaxonName == spreadsheet[j,i], "InvalidTaxonName"], ContribName = "False")
        synon_row_temp <- newXMLNode("Synonym")
        listToXML(synon_row_temp, synon_temp)  
               #add to the row entry
        addChildren(row, kids = list(synon_row_temp), append = TRUE)
        
        (distinct_taxa[,"TaxonName"] %in% spreadsheet[j,2] && distinct_taxa[,"Modification"] %in% spreadsheet[j,6])
        
        
      }}
      
      #I'm keeping the specimen code for now, because I want to re-use it for the synonymies
      
      # #if that row is in a column after 7, check for specimens and add them as well.
      # if(i > 7){
      #   for(k in 1:nrow(FAUNAL_sub)){
      #     
      #     #if Name matches this specimen's Taxon, 
      #     #and Analysis Unit matches this specimen's StratLev
      #     if(spreadsheet[j,"Name"]==FAUNAL_sub[k,"Taxon"] && 
      #        spreadsheet[2,i]==FAUNAL_sub[k,"StratigraphicLevel"]){
      #       
      #       spec_row_temp <- newXMLNode("Specimen", attrs = c(ID = k))
      #       
      #       #add to the row entry
      #       addChildren(row, kids = list(spec_row_temp), append = TRUE)
      #       
      #     } #end of specimen-matching if statement
      #     
      #   } #end of looping through specimens list
      #   
      # } #end of specimen-adding if statement
      
      #append the jth row to the row list
      rowlist[[length(rowlist)+1]]<-row
      
    } #end of row loop
    
    #skip this column if there are no rows
    if(length(rowlist) == 0) next
    
    #adding the rows as a list to the ith column
    addChildren(col, kids = list(rowlist), append = TRUE)
    
    #append the ith column to the column list
    collist[[length(collist)+1]]<-col
    
  } #end of spreadsheet loop
  
  #adds the columns to the xml spreadsheet object
  addChildren(Spreadsheet_XML, kids = list(collist), append = TRUE)
  
  addChildren(SpreadSheetBook, kids = list(Spreadsheet_XML))
  
  }# end of pfft in case of no specimens
  
  addChildren(XML_out, kids = list(SpreadSheetBook))
  

  
#Don't forget to write out the .xml file!

  #for testing:  
#saveXML(XML_out, file="test.TLX")
  
  ##***Rework the save to use the SiteName
  
  saveXML(XML_out, file = paste0("Test_Files/",SiteName, ".TLX"), prefix = '<?xml version="1.0"?>\n')
  
} #end of CU (outermost) loop

