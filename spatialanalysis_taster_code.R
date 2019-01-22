#2019 TASTER SESSION: SPATIAL ANALYSIS AND MODELLING
#Examples using UK house price data from the Land Registry
#And codepoint open geocoded postcodes

#If not installed, install first with e.g.
#Install.packages('tidyverse')
library(tidyverse)
library(lubridate)
library(raster)
library(sf)#SIMPLE FEATURES LIBRARY, DOING ALL THE SPATIAL HEAVY LIFTING
library(tmap)
library(spdep)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#30 SECOND PIPE OPERATOR PRIMER---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Normal function
sqrt(100)

#Using the pipe operator... pipe into the function.
100 %>% sqrt

#Becomes powerful when you create chains
100 %>%
  sqrt %>%
  log10

#... as we'll see below.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD HOUSING DATA, SUBSET TO LONDON----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#NOT SHOWING POSTCODE / MERGING WORKING OUT HERE...

#LOAD IN DATAFRAME OF GEOCODED HOUSE PRICES
#I'VE DONE MERGING AND GEOCODING ELSEWHERE
#Nearly seven million sales 1995-2017
#Includes a label for travel to work areas (TTWAs)
#Half a GB in memory
sales <- readRDS('data/landRegistryPricePaidTopTTWAs.rds')

#Variable names
names(sales)

#Make sure R knows dates are dates (via lubridate)
sales$year <- year(sales$date)

#We've got data from 1995 to 2017
range(sales$year)

#Just looking at London - try some others
#London: 3.4 million sales
city <- sales %>% filter(ttwa == 'London')


#Convert the (non-spatial) dataframe of sales points for London
#To spatial points sf
city.sf <- st_as_sf(city, coords = c('Eastings','Northings'))



#"HERE'S ONE I MADE EARLIER..."
#saveRDS(city.sf, 'data/city_sf.rds')
city.sf <- readRDS('data/city_sf.rds')

#THESE ARE THE PROPERTY TYPES WE HAVE:
table(city.sf$type)

#View in slides, takes a while to open
#View(city.sf)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GET COUNT OF PROPERTY TYPE PER 1KM GRID SQUARE, PLOT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#CREATE 1KM GRID-SQUARE SPATIAL DATAFRAME
#Matching extent of our postcode data points
london.raster <- raster(extent(city.sf), res = 1000)
london.grid <- rasterToPolygons(london.raster)


#Convert london grid to SIMPLE FEATURES object
#Set coordinate reference system while we're here
#27700 is British National Grid EPSG code.
london.grid <- st_as_sf(london.grid, crs = 27700) %>% 
  mutate(gridsquareID = 1:n())


#SO WE HAVE A RASTER OF GRID SQUARES
#Check they're in the same place
plot(st_geometry(london.grid))
#Just a sample of points will do - plotting them all would take A LONG TIME
plot(st_geometry(sample_n(city.sf, 1000)), add=T, col="RED")





#COUNT OF ALL HOUSE TYPES PER GRID SQUARE IN A PARTICULAR YEAR / RANGE OF YEARS
range(city.sf$year)

#Filter down to a selection of years
city.sf.subset <- city.sf %>% filter(year %in% c(2014,2015,2016))


#COUNT THE NUMBER OF SALES IN EACH GRID SQUARE
#FOR EACH TYPE OF PROPERTY
#THIS IS WHERE SIMPLE FEATURES AND PIPING START BEING AWESOME

#TAKES ABOUT A MINUTE TO RUN, SO....
count.of.type <- london.grid %>%
  st_join(city.sf.subset) %>% 
  filter(!is.na(type)) %>% #drop NAs: grid squares with no sales in around the edge
  group_by(gridsquareID,type) %>%
  summarise(count = n())


#..."HERE'S ONE I MADE EARLIER..."
#saveRDS(count.of.type,'data/count_of_type.rds')
count.of.type <- readRDS('data/count_of_type.rds')

#Take a look at the result
View(count.of.type)

#TMAP!
#Plot count of single types
typemap <- tm_shape(count.of.type) +
  tm_polygons(col = 'count', style = 'cont', palette = '-Spectral', border.alpha = 0.5) +
  tm_layout(asp = 1.5) + #set aspect ratio so we can see the legend
  tm_facets(by = 'type', free.scales = T)

#Save! Looks better.
tmap_save(typemap, "outputs/typemap.png", width=3000, unit="px")






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GET MOST COMMON (MODAL) PROPERTY TYPE PER 1KM GRID SQUARE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SAME AS BEFORE EXCEPT USING A DIFFERENT FUNCTION IN THE FINAL SUMMARISE STAGE
#Again, about a minute
modal.type <- london.grid %>%
  st_join(city.sf.subset) %>%
  filter(!is.na(type)) %>% #drop NAs. Grid squares around edge of city have no sales in them.
  group_by(gridsquareID) %>%
  summarise(mode = names(which.max(table(type))))


#"HERE'S ONE I MADE EARLIER..."
#saveRDS(modal.type,'data/modal_type.rds')
modal.type <- readRDS('data/modal_type.rds')

#See what we've got
View(modal.type)

#Loses factoring finding mode so re-factor
modal.type <- modal.type %>% 
  mutate(mode = fct_relevel(mode,
                             'flat',
                             'terrace',
                             'semi',
                             'detached'
  )) 


#OUTPUT MAP
tm_shape(modal.type) +
  tm_polygons(col = 'mode', style = 'cat', palette = '-Set1', border.alpha = 0.5) 

#FOR SAVING AS FILE
#x <- tm_shape(modal.type) +
#  tm_polygons(col = 'mode', style = 'cat', palette = '-Set1', border.alpha = 0.5) 
#tmap_save(x, "outputs/modal.png", width=1700, unit="px")



#BACK TO SLIDES FOR A BIT...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#WEIGHTS MATRIX 1: HOUSE PRICE VS EMPLOYMENT REGRESSION----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1. GET PER-WARD HOUSE PRICE SUMMARY, LINK TO EMPLOYMENT DATA IN EACH WARD

#Get house price summaries per ward from the sales data
priceSummary <- sales %>%
  group_by(year,wardcode) %>%
  summarise(meanprice = mean(price), medianprice = median(price), count = n(), ttwa = max(ttwa))



employment <- read_csv('data/percentEmployedByWard2001n2011.csv')

#year in its own column
empLong <- employment %>%
  gather(key = year, value = percentEmployed, `2001`:`2011`) %>%
  mutate(year = as.numeric(year))

#Keeps only matches with all-populated columns. So we only get 2001 and 2011.
avpriceplusemp <- inner_join(priceSummary,empLong,
                             by = c('year','wardcode'))



#"HERE'S ONE I MADE EARLIER..."
#saveRDS(avpriceplusemp,'data/avpriceplusemp2.rds')
avpriceplusemp <- readRDS('data/avpriceplusemp2.rds')
View(avpriceplusemp)


#SUBSET TO SINGLE CITY / TRAVEL TO WORK AREA
#LET'S LOOK AT SHEFFIELD AND ROTHERHAM
avpriceplusemp.city <- avpriceplusemp %>% filter(ttwa=='Sheffield & Rotherham', year == 2001)
#Some other options
#avpriceplusemp.city <- avpriceplusemp %>% filter(ttwa=='Manchester', year == 2001)
#avpriceplusemp.city <- avpriceplusemp %>% filter(ttwa=='London', year == 2001)



#SIMPLE REGRESSION ON THAT:
city.model <- lm(data = avpriceplusemp.city, formula = log(meanprice) ~ percentEmployed)
summary(city.model)

plot(log(avpriceplusemp.city$meanprice) ~ avpriceplusemp.city$percentEmployed)
abline(city.model)





#2: ADDING A WEIGHTS MATRIX TO THE REGRESSION



#WE NEED THE WARDS SPATIAL DATA TO GET A NEIGHBOUR LIST
#First, get ward spatial file
wards.spatial <- st_read('data/England_wa_1991/england_wa_1991.shp')

#Check we have all wards matching
table(avpriceplusemp.city$wardcode %in% wards.spatial$label)



#Subset to our chosen city
city.wards <- wards.spatial %>% filter(wards.spatial$label %in% avpriceplusemp.city$wardcode)

#check visually
plot(st_geometry(city.wards))

#MERGE HOUSE PRICE / EMPLOYMENT DATA TO SPATIAL FILE
#JUST TO GUARANTEE WARDS MATCH CORRECTLY
city.wards <- merge(city.wards, avpriceplusemp.city, by.x = 'label', by.y = 'wardcode')
View(city.wards)


#CONVERT TO FORM THAT SPDEP PACKAGE CAN USE - HAVE TO JUMP OUT OF SF FOR THIS
#convert to spatialpolygondataframe so we can use spdep
city.wards <- as_Spatial(city.wards)

#https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
#GIVES US A NEIGHBOUR LIST FOR EVERY WARD
neighbours <- poly2nb(city.wards)

#NOT A MATRIX YET
#Which gives us a list of neighbours for every ward...
#LOOK AT 30TH ROW
look.at.this.ward <- 30

neighbours[[look.at.this.ward]]

#So e.g.
#plot(city.wards)
plot(city.wards[look.at.this.ward,], add=T, col='RED')
plot(city.wards[neighbours[[look.at.this.ward]],], add=T, col='BLUE')


#GET THE CONTIGUITY MATRIX
#Row-normalised BY DEFAULT
contiguity.matrix <- nb2mat(neighbours)
#mx <- nb2mat(contiguity.matrix, zero.policy = T)#sometimes need this if neighbours are missing

#Weights add to 1 over rows.
apply(contiguity.matrix,1,sum)



#GET SPATIAL LAG VALUES
#MULTIPLY CONTIG MATRIX BY PERCENT EMPLOYED
#TO GET AVERAGE EMPLOYMENT IN NEIGHBOURING ZONES
spatial.lag <- contiguity.matrix %*% city.wards$percentEmployed

#JUST TO SHOW IT'S FINDING THE AVERAGE OF NEIGHBOURING ZONES
#Check ward to look at  to see if that's the average...
#HERE'S THE RESULT FROM THE WEIGHTS MATRIX...
spatial.lag[look.at.this.ward]


#AND GETTING DIRECTLY FROM THE DATA USING THE NEIGHBOURS
#Is it the same...? Tick!
sum(city.wards$percentEmployed[neighbours[[look.at.this.ward]]])/length(city.wards$percentEmployed[neighbours[[look.at.this.ward]]])


#Add to data so's we can regress it. We already know it's in the right order.
city.wards$spatial.lag <- spatial.lag


#Original model
city.model <- lm(data = city.wards@data, formula = log(meanprice) ~ percentEmployed)
summary(city.model)

#With spatial lag as a covariate
city.model <- lm(data = city.wards@data, formula = log(meanprice) ~ percentEmployed + spatial.lag)
summary(city.model)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#WEIGHTS MATRIX 2: MORAN'S I----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#let's use Moran's I to check on the spatial autocorrelation
#for the different housing types per grid square above.

#WHEN COUNTING TYPE PER GRID SQUARE
#ALSO COUNT ZEROS SO WE HAVE THE SAME SPATIAL LAYOUT FOR EACH TYPE
#AND CAN INCLUDE ZEROS IN MORANS I.
#First thing: to be able to compare, we need to include grid squares with zero counts.
#By default, dplyr's summarise drops these.
#We can use "complete" to add back in those values
#But it doesn't play very well with sf (we lose the geometry)
#So a bit more faff involved:
count.of.type.moran <- london.grid %>%
  st_join(city.sf.subset) %>% 
  filter(!is.na(type)) %>% #drop NAs: grid squares with no sales in around the edge
  group_by(gridsquareID,type) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  complete(gridsquareID,type,fill = list(count = 0))

#That outputs a plain dataframe that messes up the sf geometry
#Merge into the london.grid sf file to get it back
count.of.type.moran <- inner_join(london.grid %>% dplyr::select(-layer), count.of.type.test %>% dplyr::select(-geometry), by = "gridsquareID")

#WE USE THE SPDEP PACKAGE TO FIND MORAN'S I
#SPDEP NEEDS TO USE AN OLDER SPATIAL FORMAT 
#SO WE CONVERT TO A SPATIALPOLYGONSDATAFRAME...
count.of.type.moran <- as_Spatial(count.of.type)




#"HERE'S ONE I MADE EARLIER..."
#saveRDS(count.of.type.moran,'data/countoftypemoran.rds')
count.of.type.moran <- readRDS('data/countoftypemoran.rds')
View(count.of.type.moran@data)



#BECAUSE THE GEOGRAPHY REPEATS,
#GET NEIGHBOURS LIST ONLY FOR ONE OF THEM
#https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
neighbours <- poly2nb(count.of.type.moran[count.of.type.moran$type=='flat',])

#For Moran's I, we use binary (style="B"): neighbours indicated by ones.
#Also in list form actually, rather than matrix
mx <- nb2listw(neighbours, style="B", zero.policy = T)


#SUBSET DATASET JUST TO FLATS
#Find Moran's I for a single housing type
#We can't use dplyr-style code here
flats <- count.of.type.moran[count.of.type.moran$type=='flat',]

#Moran's I just for flats...
#Need zero policy due to a few isolated grid squares
moran.test(flats$count, mx, zero.policy = T)


#Run for each house type using purrr
count.of.type.moran %>% 
  split(.$type) %>% #split into each housing type
  map_dbl(~moran.test(.$count, mx, zero.policy = T)$estimate[1])#map each of those subsets to a function








