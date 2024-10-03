##_______________________________________________________________________________________________________________________________
# Skate pilot model exploration, Sophie Loca 15/04/21

# Protocol according to Zuur et al. 2010 - protocol for data exploration to avoid common statistical problems
##_______________________________________________________________________________________________________________________________

rm(list = ls())
source("HighstatLibV13.R")
library(ggplot2)
library(raster)
library(dplyr)
library(tidyr)
library(rWind)
library(corrplot)

#Load packages and support files

library(lattice)

library(ggstatsplot)
library(rgdal)
library(sp)
library(gstat)

library(reshape)
library(fields)
library(maps)
library(maptools)
library(mapdata)

data("worldHiresMapEnv")

##_______________________________________________________________________________________________________________________________

# Import your data set
Skate <- read.csv("data/data_f_n.csv")
names(Skate)
str(Skate)
Skate <- Skate %>% drop_na()
Skate$bath <- as.numeric(Skate$bath)
Skate$distance <- as.numeric(Skate$distance)
Skate$hauldur <- as.numeric(Skate$hauldur)
Skate$rockhopper <- as.factor(Skate$rockhopper)

# Get UTM for spatial stuff later
LL <- LongLatToUTM(x = Skate$haullon, 
                   y = Skate$haullat, 
                   zone = 29,
                   Hemisphere = "north")
Skate$xkm <- (LL$X)/1000
Skate$ykm <- (LL$Y)/1000



# Plot your data
map <-
  ggplot(data = Skate , aes(x=haullon, y=haullat)) +
  geom_point() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic()
map


##_______________________________________________________________________________________________________________________________

# 1. Outliers


# Outliers in the response variable and the covariates
MyVar  <- c("haullat", "haullon", "hauldur", "groundspeed", "dcoast", "current_mi", "btemp_mi", "bath", "distance", "xkm")
Mydotplot(Skate[, MyVar]) 


# Identify obvious outliers and remove. You can do this by calculating statistical outliers (option 1) or by removing the obvious
# ones by eye (option 2)

# Option 1

outliers <- boxplot(Skate$bath, plot=TRUE)$out
Skate<- Skate[-which(Skate$dcoast %in% outliers),]

# Option 2

Skate <- filter(Skate, cpue_ro < 20)
Skate <- filter(Skate, bath >-400)
Skate <- filter(Skate, distance <6000)
Skate <- filter(Skate, ground_speed > -9)
Mydotplot(Skate[, MyVar])


##_______________________________________________________________________________________________________________________________

# 2. Homogeneity of variance
# Most anova tests and regression models assume variance within groups are the same (or similar). You can create
# a series of boxplots in order to visually judge any potential violation. In regression-type models, you can verify
# this using the residuals of the model (i.e. plotting residuals vs fitted values) and making a similar set of 
# conditional boxplots for the residuals. As a solution you can transform the response variable or apply other 
# techniques.

ggplot (Skate, aes(x=, y = current_mi)) +
  geom_boxplot() +
  ylab("current")+
  facet_wrap(~skate_gen)+
  theme_classic()

ggplot (Skate, aes(x=, y = dcoast)) +
  geom_boxplot() +
  ylab("dcoast")+
facet_wrap(~skate_gen)+
  theme_classic()

ggplot (Skate, aes(x=, y = bath)) +
  geom_boxplot() +
  ylab("bath")+
  facet_wrap(~skate_gen)+
  theme_classic()

ggplot (Skate, aes(x=, y = btemp_mi)) +
  geom_boxplot() +
  ylab("temp")+
  facet_wrap(~skate_gen)+
  theme_classic()

ggplot (Skate, aes(x=, y = pp)) +
  geom_boxplot() +
  ylab("pp")+
  facet_wrap(~skate_gen)+
  theme_classic()

ggplot (Skate, aes(x=, y = groundspeed)) +
  geom_boxplot() +
  ylab("groundspeed")+
  facet_wrap(~skate_gen)+
  theme_classic()

ggplot (Skate, aes(x=, y = distance)) +
  geom_boxplot() +
  ylab("distance")+
  facet_wrap(~skate_gen)+
  theme_classic()


##_______________________________________________________________________________________________________________________________

# 3. Normality of data
# A histogram is a good start, though make sure to check how it may vary per group - sometimes skewedness is only prominent 
# within one subgroup.

ggplot (Skate, aes(x= dcoast)) +
  geom_histogram(bins = 10) + 
  theme_classic() 


ggplot (Skate, aes(x= btemp_mi)) +
  geom_histogram(bins = 10) + 
  theme_classic() 


ggplot (Skate, aes(x= pp)) +
  geom_histogram(bins = 20) + 
  theme_classic() 


ggplot (Skate, aes(x= bath)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

ggplot (Skate, aes(x= distance)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

ggplot (Skate, aes(x= groundspeed)) +
  geom_histogram(bins = 40) + 
  theme_classic() 

ggplot (Skate, aes(x= current_mi)) +
  geom_histogram(bins = 40) + 
  theme_classic() 

# Looks like these should all be standardised, as expected really, and is good practice for continuous variables.

##_______________________________________________________________________________________________________________________________

# 4. Zero-inflation

zero <- sum(Skate$skate_gen == "0")
non_zero <- sum(Skate$skate_gen != "0")
(zero/(zero + non_zero))*100

##_______________________________________________________________________________________________________________________________

# 5. Collinearity

MyVar  <- c("hauldur", "groundspeed", "dcoast", "current_mi", "btemp_mi", "bath", "distance")
MyVar  <- c("hauldur", "groundspeed", "dcoast", "current_mi", "btemp_mi", "bath")

Mypairs(Skate[,MyVar])
VIF <- corvif(Skate[,MyVar])
write.csv(VIF, "export/VIF2.csv")

my_data <- Skate[, c(7,11,12,15,16, 17, 18)]
colnames(my_data) <- c("Haul Duration", "Haul Distance"," Ground Speed", "Distance to Coast", "Depth", "Bottom Temperature",
                  "Current Speed") 
my_data
res <- cor(my_data)

pdf(file = "corrplot.pdf", width=20, height=15)
corrplot.mixed(res, upper = 'ellipse',lower='number', tl.pos= "lt", tl.srt = 45, tl.col = 'black', number.cex = 2, tl.cex = 2, cl.cex = 2)
dev.off()

citation("corrplot")
##_______________________________________________________________________________________________________________________________

# 6. Relationships between the response variable and covariates

ggplot(Skate, aes(x = current_mi, y = skate_gen))+
  geom_point()+
  geom_smooth(method = "loess")

ggplot(Skate, aes(x = mud, y = skate_gen))+
  geom_point()+
  geom_smooth(method = "loess")

ggplot(Skate, aes(x = btemp_mi, y = skate_gen))+
  geom_point()+
  geom_smooth()

ggplot(Skate, aes(x = bath, y = skate_gen))+
  geom_point()+
  geom_smooth()

ggplot(Skate, aes(x = dcoast, y = skate_gen))+
  geom_point()+
  geom_smooth()

ggplot(Skate, aes(x = current, y = cpue_01))+
  geom_point()+
  geom_smooth()



# Plot cpue_no_per_hour versus the selected covariates - with smoother.
library(dplyr)

GatherTheseCols <- c("hauldur", "groundspeed", "current_mi", "btemp_mi", "bath", "haullon", "haullon")

Skate_long <- gather(data = Skate, 
                      key = "ID", value = "AllX", 
                      GatherTheseCols, 
                      factor_key = TRUE) 

# The new objectSkate_long is in the so-called 'long format'. It contains a lot of rows because the original data set 
# is repeated various times. We use this modified data object in a ggplot2 function to make a multi-panel scatterplot; 
# see the figure below. 


p <- ggplot()+ 
  geom_point(data = Skate_long, aes(y = cpue_ro , x = AllX))+
  geom_smooth(data = Skate_long,aes(y = cpue_ro, x = AllX)) +
  xlab("Covariates") + ylab("cpue_no_per_hour")+
  theme(text = element_text(size=12), legend.position="none")+
  theme(axis.text.x = element_text(size = 9, angle=45,hjust = 0.9))+
  facet_wrap(.~ID, scale = 'free_x', ncol = 4)               
p




