## validate soilgrids100 gg and pscs predictions using independent validation points
## code by Colby Brungard (cbrung@nmsu.edu)

# Set working directory and load necessary packages
setwd("D:/SoilGrids100")
library(plyr)
library(dplyr)
library(caret)
library(sp)
library(raster)
library(rgdal)
library(spam)

# Load validation data
BeaverCountyUT <- read.csv("./Validation Data/BeaverCounty_UT/BC_SiteData_soilGridsValid.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
BeaverCountyUT$area <- rep('BeaverCountyUT', n = nrow(BeaverCountyUT))

BeaverCreekWY <- read.csv("./Validation Data/BeaverCreek_WY/BC_SiteData_soilGridValid.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
BeaverCreekWY$area <- rep('BeaverCreekWY', n = nrow(BeaverCreekWY))

BoundaryWatersMN <- read.csv("./Validation Data/BoundaryWaters_MN/BW_SiteData_soilGridValid.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
BoundaryWatersMN$area <- rep('BoundaryWatersMN', n = nrow(BoundaryWatersMN))

CRCUT<- read.csv("./Validation Data/CRC_UT/MillerPlotstoColbywgs.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
CRCUT$area <- rep('CRCUT', n = nrow(CRCUT))

FortBlissNM <- read.csv("./Validation Data/FortBliss_NM/FB_SiteData_soilGridValid.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
FortBlissNM$area <- rep('FortBlissNM', n = nrow(FortBlissNM))

JuabCountyUT <- read.csv("./Validation Data/JuabCounty_UT/JC_SiteData_soilGridsValid.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
JuabCountyUT$area <- rep('JuabCountyUT', n = nrow(JuabCountyUT))

Maine <- read.csv("./Validation Data/Maine/ME_SiteData_soilGridsValid.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
Maine$area <- rep('Maine', n = nrow(Maine))

MillardCountyUT <- read.csv("./Validation Data/MillardCounty_UT/MC_SiteData_soilGridsValid.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
MillardCountyUT$area <- rep('MillardCountyUT', n = nrow(MillardCountyUT))

SnakeValleyUT <- read.csv("./Validation Data/SnakeValley_UT/SV_SiteData_soilGridsValid.csv", stringsAsFactors=FALSE, na.strings = c('', ' '), sep = ',', strip.white = TRUE)
SnakeValleyUT$area <- rep('SnakeValleyUT', n = nrow(SnakeValleyUT))

# Combine into one dataframe and factor appropriate columns
val <- rbind.fill(BeaverCountyUT,
             BeaverCreekWY, 
             BoundaryWatersMN,
             CRCUT,
             FortBlissNM,
             JuabCountyUT,
             Maine,
             MillardCountyUT,
             SnakeValleyUT)

val$greatgroup <- as.factor(val$greatgroup)
val$particlesizeclass <- as.factor(val$particlesizeclass)
val$area <- as.factor(val$area)

# Some observations contain only partial data. Split into seperate validation datasets and subset to remove missing observations
gg.val <- val[, -5]
gg.val2 <- na.omit(gg.val) # 1994 validation observations

psc.val <- val[, -4]
psc.val2 <- na.omit(psc.val) # 2026 validation observations


# Check if classes in validation are not in the soilgrids100 legend.
#greatgroups
gg.levs <- read.csv("./Predictions/TaxGG legend.csv")
sg.gg <- data.frame(gg.levs$Class) # must be a dataframe for anti_join to work
names(sg.gg) <- 'greatgroup'
vl.gg <- data.frame(gg.val2$greatgroup) # must be a dataframe for anti_join to work
names(vl.gg) <- 'greatgroup'
unique(anti_join(vl.gg, sg.gg)) # list the factor levels in the validation data that do not match the gg legend 

#particlesizeclass
psc.levs <- read.csv("./Predictions/PSCS_M_100m.tif.csv") #I first used Excel::PROPER() to format capitilization
sg.psc <- data.frame(psc.levs$Class) 
names(sg.psc) <- 'particlesizeclass'
vl.psc <- data.frame(psc.val2$particlesizeclass) 
names(vl.psc) <- 'particlesizeclass'
unique(anti_join(vl.psc, sg.psc)) # list the factor levels in the validation data that do not match the gg legend

# Rename classes with no counterpart in the soilgrids legend to match those in the soilgrids legends. For greatgroup mismatches were mostly misspelled words. particlesizeclass mismatches were because soilgrids lumped X over sandy or sandy-skeletal.  
gg.val2$greatgroup <- revalue(gg.val2$greatgroup, c(
  'Aquicambid' = 'Aquicambids',
  'Calciargid' = 'Calciargids', 
  'Endoaquent' = 'Endoaquents', 
  'Halargids' = 'Haplargids',
  'Haplargid' = 'Haplargids',
  'Haplocalcid' = 'Haplocalcids',
  'Haplocambid' = 'Haplocambids',
  'Hapudalfs' = 'Hapludalfs',
  'Natrargid' = 'Natrargids', 
  'Torrirothents' = 'Torriorthents'))

psc.val2$particlesizeclass <- revalue(psc.val2$particlesizeclass, c(
'Fine-Silty Over Sandy' = 'Fine-Silty Over Sandy Or Sandy-Skeletal',
'Loamy-Skeletal Over Sandy' = 'Loamy-Skeletal Over Sandy Or Sandy-Skeletal',
'Fine-Silty Over Sandy-Skeletal' = 'Fine-Silty Over Sandy Or Sandy-Skeletal',
'Fine-Loamy Over Sandy' = 'Fine-Loamy Over Sandy Or Sandy-Skeletal',
'Coarse-Loamy Over Sandy-Skeletal' = 'Coarse-Loamy Over Sandy Or Sandy-Skeletal',
'Loamy Over Sandy-Skeletal' = 'Loamy-Skeletal Over Sandy Or Sandy-Skeletal'))

# Convert to spatial points dataframe and Write to shapefile for travis
coordinates(gg.val2) <- ~ longitude+latitude
proj4string(gg.val2) <- '+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs' 
coordinates(psc.val2) <- ~ longitude+latitude
proj4string(psc.val2) <- '+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs' 

#writeOGR(gg.val2, "./Validation Data/shapefiles", "greatgroups", driver="ESRI Shapefile")
#writeOGR(psc.val2, "./Validation Data/shapefiles", "particlesizeclass", driver="ESRI Shapefile")

# Load soilgrids predictions
gg <- raster("./Predictions/TAXgg_M_100m.tif")
psc <- raster("./Predictions/PSCS_M_100m.tif")

# Extract predictions at each validation point then rename the extracted values to enable joining
ggE <- extract(gg, gg.val2, sp = TRUE)
names(ggE@data)[4] <- 'Value'
pscE <- extract(psc, psc.val2, sp = TRUE) #Currently only Ashy is predicted. 
names(pscE@data)[4] <- 'Value'

# Remove points that are within 250 m of each other to avoid psuedo-replication in validation statistics
gg.dist <- spDists(ggE)

gg.dist[1, min(gg.dist[,1])]


s







































# Convert extracted (i.e., predicted) numbers to class names in the legend
gg.pred <- join(ggE@data, gg.levs, by = 'Value', type = 'left')

# Calculate confusion matricies
# Inorder to calculate meaningful confusion matricies I had to match the existing factor levels between predicted and observed, then drop all factor levels that were not used. This required a custom function
cm.fun <- function(x){ 
  notdrop <- with(x, unique(c(levels(droplevels(greatgroup)), levels(droplevels(Class)))))
  x$greatgroup <- factor(x$greatgroup, levels = notdrop)
  x$Class <- factor(x$Class, levels = notdrop)
  CM <- confusionMatrix(x$Class, x$greatgroup)
  return(CM)
}

# All validation points 
cm.all <- cm.fun(gg.pred)

# By individual verification region
cm.area <- dlply(gg.pred, .(area), .fun = cm.fun)

# Write to .csv

#misc




#Use the nearestdist function in the spam package to get distances <=90m.
#This creates an object of the class spam
#Zeros indicate points with no neighbors within 250 m. 
gg.dist <- round(nearest.dist(ggE@coords, delta = 250.1, upper = F))
#Use the triplet function to get the indicies (row, column) and the value from the spam object.
#Values column = distance to nearest point, 0 indicates no points closer than 250 m
gg.tri <- as.data.frame(triplet(gg.dist, tri = TRUE))


# Retain points that are < 250 m to the nearst neighbor  729 points are < 250 m 
gg.nz <- gg.tri[gg.tri[,3] !=0, ]

#Because some of the points are closer than 250m to several other points I need
#to find only those points (rows) that are unique. This returns 473 rows.

gg.sub <- ggE[!unique(gg.nz[,1]), ]

duplicated(gg.nz[,1])

#Subset the bsc by distances. 
#Note that the row numbers will not be identical because incomplete cases have
#already been removed from the bsc dataset.
#Also it is possible that I could still include some of the points from the
#test set in the training set because by removing them other points would 
#then not be < 90m. I could do this by finding all of the points that were 
#non unique (in the above code) but I'm not sure that I want to do this right
#now. 

gg.sub <- ggE[-gg.nz.u, ]
write.csv(gg.sub, "./gg.sub.csv")
gg.sub.in <- ggE[gg.nz.u, ]
write.csv(gg.sub.in, "./gg.sub.in.csv")

