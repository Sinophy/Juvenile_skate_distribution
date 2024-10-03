
rm(list = ls())

#Load packages and support files
source("HighstatLibV13.R")
source("extra_inla_fun.R")
library(dplyr)
library(INLA)
library(brinla)
library(mgcv)
library(lattice)
library(rWind)
library(gam)
library(gstat)
library(rgeos)
library(fields)
library('maps')
library('maptools')
library('mapdata')
library(ggmap)
library(raster) 
library(ggregplot)
library(rgdal)
library(tidyr)
library(rmarkdown)
install.packages('tinytex')
tinytex::install_tinytex()



# Import the data and some small processing
Skate <- read.csv("data/data_f.csv")

# Standardize each continuous covariate.

Skate$pp.std       <- MyStd(Skate$pp)
Skate$temp.std      <- MyStd(Skate$btemp_mi)
Skate$current.std   <- MyStd(Skate$current_mi)
Skate$groundspeed.std      <- MyStd(Skate$groundspeed)
Skate$hauldur.std   <- MyStd(Skate$hauldur)
Skate$dcoast.std   <- MyStd(Skate$dcoast)
Skate$rockhopper <- as.factor(Skate$rockhopper)
Skate$bath.std <- MyStd(Skate$bath)

# Get UTM for spatial stuff later
LL <- LongLatToUTM(x = Skate$haullon,  
                   y = Skate$haullat, 
                   zone = 29,
                   Hemisphere = "north")
Skate$xkm <- (LL$X)/1000
Skate$ykm <- (LL$Y)/1000


##______________________________________________________________________________
# Some preparation steps

# Distances between sites
Loc<- cbind(Skate$xkm, Skate$ykm)
D   <- dist(Loc)


# Figure 21.5
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
hist(D / 1000, 
     freq = TRUE,
     main = "", 
     xlab = "Distance between sites (km)",
     ylab = "Frequency")


RangeGuess <- 0.1 * 1000     
MaxEdge <-  RangeGuess/5
ConvHull <- inla.nonconvex.hull(Loc, convex=-0.04)

mesh <- inla.mesh.2d(loc.domain = coastline,
                     max.edge = 25,
                     boundary = inla.mesh.segment(Coast.rev),
                     crs = inla.CRS("lambert_norm"))

mesh$n

# plot
par(mfrow = c(1,1), mar=c(1, 1, 1, 1))
plot(mesh, asp = 1)
points(Loc, col = 2, pch = 16, cex = 1)


# Export mesh to shapefiles for plotting
mesh_tf <- inla.spTransform(mesh1, inla.CRS("lambert", passthrough = FALSE))
meshsp1<- inla.mesh2sp(mesh_tf)
summary(meshsp1)
crs(meshsp)
class(meshsp1$vertices)
class(meshsp1$triangles)

writeOGR(meshsp$triangles, dsn = 'export', layer = "mesh_triangles", driver = "ESRI Shapefile")
meshpoints <- as.data.frame(meshsp$vertices)
write.csv(meshpoints, "export/meshpoints.csv")

#_______________________________________________________________________________
# Define spde for Q1

A.est  <- inla.spde.make.A(mesh, loc = as.matrix(Loc));dim(A.est)

test <- lm(skate_gen ~ 1, data = Skate)
summary(test)$sigma


# Define the SPDE.
spde <- inla.spde2.pcmatern(mesh, 
                            prior.range = c(0.05 * 1000, 0.0001), 
                            prior.sigma = c(0.3482299, 0.05))


# Define the spatial field.
w.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spde$n.spde)

##______________________________________________________________________________

# Combine the response variable, projector matrix, covariates into a stack

N <- nrow(Skate)

est <- inla.stack(data = list(y = Skate$skate_gen), 
                  A = list(A.est, 1),                 
                  effects = list(w=1:spde$n.spde,
                                 list(Intercept    = 1,
                                 dcoast.std        = Skate$dcoast.std,
                                 current.std       = Skate$current.std,
                                 bath.std          = Skate$bath.std
                                 temp.std          = Skate$temp.std,
                                 hauldur.std       = Skate$hauldur.std,
                                 groundspeed.std   = Skate$groundspeed.std,
                                 rockhopper        = Skate$rockhopper,
                                 xkm               = Skate$xkm
                                 )),
                                 tag = 'dat')


#______________________________________________________________________________________________________________________________________________________________
# 1. Bernoulli GLM
#______________________________________________________________________________________________________________________________________________________________

f1 <- y ~ -1 + Intercept + dcoast.std + current.std + groundspeed.std + hauldur.std + temp.std + xkm+
  f(rockhopper, model = "iid") 

B1 <- inla(f1,
           family = "binomial", 
           data = inla.stack.data(est),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(est)))

# Compare models with depth vs temp and dcoast

f12 <- y ~ -1 + Intercept + bath.std + current.std + groundspeed.std + hauldur.std + xkm+
  f(rockhopper, model = "iid") 

B12 <- inla(f12,
           family = "binomial", 
           data = inla.stack.data(est),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(est)))

dic  <- as.data.frame(c(B1$dic$dic, B12$dic$dic))   
waic <- as.data.frame(c(B1$waic$waic, B12$waic$waic))  
rownames(dic) <- c("Bernoulli GLM - bathymetry variable", 
                   "Bernoulli GLM - bottom temperature and distance to coast")

rownames(waic) <- c("Bernoulli GLM - bathymetry variable", 
                   "Bernoulli GLM - bottom temperature and distance to coast")
dic
waic


# Get fitted values and residuals 
N    <- nrow(Skate)
Pi1  <- B1$summary.fitted.values[1:N,"mean"]
ExpY <- Pi1
varY <- Pi1 * (1 - Pi1)
E1   <- (Skate$skate_gen - ExpY) / sqrt(varY)


##______________________________________________________________________________

# 2. Plot residuals vs each covariate (in and not in the model).

# Check for non-linear patterns in the residuals.
MyVar <- c("dcoast", "current_mi", "groundspeed", "hauldur", "btemp_mi", "haullon", "haullat", "rockhopper", "xkm")

Skate$E1 <- E1
MyMultipanel.ggp2(Z = Skate, 
                  varx = MyVar, 
                  vary = "E1", 
                  ylab = "Residuals",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = TRUE)



# 1. Visualise the residuals on a map
MyCex <-  4 * abs(E1) /max(E1)
MyCol <- ifelse(E1 >=0, 4, 1) #Blue for positive residuals and
#Black for negative residuals
xyplot(haullat ~ haullon,
       data = Skate,
       pch = 1,
       cex = MyCex,
       col = MyCol)

# 2. Make a variogram of the residuals.

MyData <- data.frame(E1 = E1, Xkm = Skate$xkm, Ykm = Skate$ykm)
coordinates(MyData) <- c("Xkm", "Ykm")
V <- variogram(E1 ~ 1, MyData, cressie = TRUE)
plot(V, 
     main = "", 
     xlab = list(label = "Distance", cex = 1.5), 
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16,
     col = 1,
     cex = 1.5)

# There seems to be strong spatial correlation in the residuals


t1 <- gam(E1 ~ s(dcoast), data = Skate)
summary(t1) # You want a small R^2 value (deviance explained) and a small edf value (1) and pvalue > 0.005
plot(t1) # You want a straight line - no residual information 

t2 <- gam(E1 ~ s(current_mi), data = Skate)
summary(t2)
plot(t2)

t3 <- gam(E1 ~ s(btemp_mi), data = Skate)
summary(t3)
plot(t3)

t4 <- gam(E1 ~ s(groundspeed), data = Skate)
summary(t4)
plot(t4)

t5 <- gam(E1 ~ s(hauldur), data = Skate)
summary(t5)
plot(t5)

t6 <- gam(E1 ~ s(pp), data = Skate)
summary(t6)
plot(t6)


t7 <- gam(E1 ~ s(xkm), data = Skate)
summary(t7)

t0 <- gam(E1 ~ 1, data = Skate)

AIC(t0, t1, t2, t3, t4, t5, t6, t7)


#______________________________________________________________________________________________________________________________________________________________
# 2. Bernoulli GAM - no smoothers needed
#______________________________________________________________________________________________________________________________________________________________

#______________________________________________________________________________________________________________________________________________________________
# 3. Bernoulli GLM + SPDE
#______________________________________________________________________________________________________________________________________________________________


f3 <- y ~ -1 + Intercept + dcoast.std + current.std + groundspeed.std + hauldur.std + temp.std + xkm+
  f(rockhopper, model = "iid") +
  f(w, model = spde) 


B3 <- inla(f3,
           family = "binomial", 
           data = inla.stack.data(est),
           control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(est)))

dic  <- as.data.frame(c(B1$dic$dic, B3$dic$dic))   
waic <- as.data.frame(c(B1$waic$waic, B3$waic$waic))  
rownames(dic) <- c("Bernoulli GLM", 
                   "Bernoulli GLM + SRF")

rownames(waic) <- c("Bernoulli GLM", 
                   "Bernoulli GLM + SRF")
dic
waic

#______________________________________________________________________________________________________________________________________________________________
# 4. Model validation
#______________________________________________________________________________________________________________________________________________________________

# Get fitted values and residuals 
N    <- nrow(Skate)
Pi3  <- B3$summary.fitted.values[1:N,"mean"]
ExpY <- Pi3
varY <- Pi3 * (1 - Pi3)
E3   <- (Skate$skate_gen - ExpY) / sqrt(varY)


# Check for overdispersion
p   <- nrow(B3$summary.fixed) 
Dispersion <- sum(E3^2) / (N - p)
Dispersion


# 1. Visualise the residuals on a map
MyCex <-  4 * abs(E3) /max(E3)
MyCol <- ifelse(E3 >=0, 4, 1) #Blue for positive residuals and
#Black for negative residuals
xyplot(haullat ~ haullon,
       data = Skate,
       pch = 1,
       cex = MyCex,
       col = MyCol)

# 2. Make a variogram of the residuals.

MyData <- data.frame(E3 = E3, Xkm = Skate$xkm, Ykm = Skate$ykm)
coordinates(MyData) <- c("Xkm", "Ykm")
V <- variogram(E3 ~ 1, MyData, cressie = TRUE)
plot(V, 
     main = "", 
     xlab = list(label = "Distance", cex = 1.5), 
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16,
     col = 1,
     cex = 1.5)


# Inspect the results of the models

# Plot the results of the model with and without the spatial correlation side by side.
# We compare the betas of the models B2 (Bernoulli GAM), and B3 (Bernoulli GAM + SRF)

#  First extract the betas and give them proper names
Out2 <- B1$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out3 <- B3$summary.fixed[,c("mean", "0.025quant", "0.975quant")]

rownames(Out2) <- rownames(Out3) <- B2$names.fixed

# Names for the models:
MyNames <- c("Bernoulli GLM",  "Bernoulli GLM + SRF")

# Execute one of our support functions
MyCompareBetasofModels(AllModels = list(Out2, Out3), 
                       ModelNames = MyNames)

ggsave("export/betascompare1.png", dpi = 600)

# The spatial GLM has wider 95% CIs. That is to be expected.


#_______________________________________________________________________________

# Hyperparameters.

SpatField.w <- inla.spde2.result(inla = B3,
                                 name = "w",
                                 spde = spde,
                                 do.transfer = TRUE)

Kappa <- inla.emarginal(function(x) {x}, 
                        SpatField.w$marginals.kappa[[1]] )

Sigma_u <- inla.emarginal(function(x) {sqrt(x)}, 
                          SpatField.w$marginals.variance.nominal[[1]] )

Range <- inla.emarginal(function(x) {x}, 
                        SpatField.w$marginals.range.nominal[[1]] )

c(Kappa, Sigma_u, Range)
Kappa
Sigma_u
Range       #Distance at which the correlation diminishes


# This is perhaps a nicer graph to make and present.
# Show correlation structure
# First we obtain the locations of each point of the mesh.
LocMesh <- mesh$loc[,1:2]

# And then we calculate the distance between each vertex.
D <- as.matrix(dist(LocMesh))

# Using the estimated parameters from the model (see above)
# we can calculate the imposed Matern correlation values.
d.vec <- seq(0, max(D), length = 100)      
Cor.M <- (Kappa * d.vec) * besselK(Kappa * d.vec, 1) 
Cor.M[1] <- 1

# Which we plot here:
par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(x = d.vec, 
     y = Cor.M, 
     pch = 16, 
     type = "l", 
     cex.lab = 1.5,
     xlab = "Distance", 
     ylab = "Correlation",
     xlim = c(0, 2500))
abline(h = 0.1, lty = 2)

ggsave("export/correlaion.png", dpi = 600)

#______________________________________________________________________________________________________________________________________________________________
# 5. Model selection - no selection
#______________________________________________________________________________________________________________________________________________________________

# Final model - S1

final <- y ~ -1 + Intercept + dcoast.std + current.std + groundspeed.std + hauldur.std + temp.std + xkm +
  f(rockhopper, model = "iid") +
  f(w, model = spde) 

S1 <- inla(final,
           family = "binomial", 
           data = inla.stack.data(est),
           control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(est)))

# Numerical output for the betas
BetaS1 <- S1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] 
print(BetaS1, digits = 2)

write.csv(BetaS1, "export/output.csv")


#_______________________________________________________________________________

# Simulation study for zeros
final

# Covariate matrix

Covariates <- data.frame(
  Intercept    = rep(1, N),
  dcoast.std = Skate$dcoast.std,
  current.std = Skate$current.std,
  hauldur.std = Skate$hauldur.std,
  groundspeed.std = Skate$groundspeed.std,
  temp.std = Skate$temp.std,
  xkm = Skate$xkm
)


# Simulate 1000 sets of betas and ws from the model:
NSim <- 1000
SimData <- inla.posterior.sample(n = NSim, result = S1)
names(SimData[[1]])
SimData[[1]]$latent


# For each of these simulated betas and random effects, we will calculate fitted values and simulate the response variable.
# First we need to determine on which rows in SimData[[1]]$latent the betas are.

#1.Determine the name of the last regression parameter (LastBeta)
nd <- length(rownames(SimData[[1]]$latent))
LastBeta <- rownames(SimData[[1]]$latent)[nd]

#2.See whether its last 2 characters equal ':1'. (Last2Character)
substrRight <- function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }
Last2Character <- substrRight(LastBeta, 2)

#3. If so, then add ':1' to our beta names (BetasInModel), resulting in BetasInINLA
BetasInModel <- rownames(S1$summary.fixed) #                                        <--Change the name 'I2.sim' for your model
MyID         <- function(x){ which(rownames(SimData[[1]]$latent) == x) }
BetasInINLA <- BetasInModel
if (Last2Character == ":1") { BetasInINLA  <- paste(BetasInModel, ":1", sep ="") }

#4.Determine the row numbers in SimData[[1]]$latent where we can find our regression parameters (BetasInINLA): BetaRows
BetaRows <- lapply(BetasInINLA, MyID)
BetaRows <- as.numeric(BetaRows)
BetaRows

# Now we know where the simulated betas are.

# Start a loop to extract betas, calculate the fitted values and simulate count data from the model.
N  <- nrow(Skate)
Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)
Xmat <- as.matrix(Covariates)

for (i in 1: NSim){
  Betas <- SimData[[i]]$latent[BetaRows]
  eta         <- Xmat %*% Betas 
  mu.i[,i]    <- exp(eta)
  Ysim[,i]    <- rpois(n = nrow(Skate), lambda = mu.i[,i])
}

table(Ysim[,1])
table(Ysim[,2])
table(Ysim[,3])

# Now we have 1000 simulated data sets from the model.
# What shall we do with these simulated data sets?
# We could calculate the number of zeros in each of the 1,000
# data sets.
sim_zeros <- vector(length = NSim)
for(i in 1:NSim){
  sim_zeros[i] <- sum(Ysim[,i] == 0)
}

real_zeros <- sum(Skate$skate_gen == 0)


library(ggplot2)
ggplot() +
  aes(sim_zeros)+
  geom_histogram(bins=50) +
  geom_point(aes(x = real_zeros, y = 0), col = "red", size = 4)+ 
  labs(x = "Simulated zeros", y = "Frequency")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),    # This theme bit removes the gridlines
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("export/zeros1.png", dpi = 600)

#The red dot is the number of zeros in the original data set. 
# The data simulated from the Poisson model does not contain enough zeros.

#_______________________________________________________________________________


# Calculate the dispersion statistic for each of these 1,000 data sets.
Disp   <- vector(length = NSim)               #Create space
N      <- nrow(Skate)                         #Sample size
Npar  <- nrow(B3$summary.fixed)               #Number of parameters
for(i in 1:NSim){
  ei <- (Ysim[,i] - mu.i[,i]) / sqrt(mu.i[,i])
  Disp[i] <- sum(ei^2) / (N - Npar)
}
Disp

# Let's plot this in a histogram


ggplot() + 
  aes(Disp)+
  geom_histogram(bins=200) +
  geom_point(aes(x = Dispersion, y = 0), col = "red", size = 4)+ 
  xlim(0, 5)+
  ylim(0,50)+
  labs(x = "Simulated dispersion statistics", y = "Frequency")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),    # This theme bit removes the gridlines
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("export/disp.png", dpi = 600)


# The histogram shows the dispersion for simulated Bernoulli data sets.

#______________________________________________________________________________________________________________________________________________________________
# 6. Spatial random field
#______________________________________________________________________________________________________________________________________________________________


# Plot spatial random field

PlotField2 <- function(field, mesh, ContourMap, xlim, ylim, Add=FALSE, MyMain, ...){
  stopifnot(length(field) == mesh$n)
  # Plotting region to be the same as the study area polygon
  if (missing(xlim)) xlim <- ContourMap@bbox[1, ] 
  if (missing(ylim)) ylim <- ContourMap@bbox[2, ]
  
  # inla.mesh.projector: it creates a lattice using the mesh and specified ranges. 
  proj <- inla.mesh.projector(mesh, 
                              xlim = xlim, 
                              ylim = ylim, 
                              dims = c(300, 300))
  # The function inla.mesh.project can then 
  # be used to project the w's on this grid.
  field.proj <- inla.mesh.project(proj, field)
  
  # And plot the whole thing
  image.plot(list(x = proj$x, 
                  y = proj$y,
                  z = field.proj), 
             xlim = xlim, 
             ylim = ylim,
             asp = 1,
             add = Add,
             main = MyMain,
             ...)  
}


w <- S1$summary.random$w$mean

par(mfrow = c(1,1), oma=c( 0,0,0,0), mar = c(4,4,1,1)) # margin of 4 spaces width at right hand side
w.pm <- S1$summary.random$w$mean
PlotField2(field = w.pm, 
           mesh = mesh, 
           xlim = range(mesh$loc[,1]), 
           ylim = range(mesh$loc[,2]),
           MyMain = "")
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)


# Create df and export

LocMesh <- mesh$loc[,1:2] # Matrix of mesh locations in km

rf_df <- make_prediction_map(LocMesh, w.pm, remove_obs = T)

write.csv(rf_df,'export/rf.csv')


Dispersion
#______________________________________________________________________________________________________________________________________________________________
# 7. Plot fixed parameters and hyperparameters
#______________________________________________________________________________________________________________________________________________________________

summary(S1)
dic  <- as.data.frame(c(B2$dic$dic, B3$dic$dic, S1$dic$dic))   
rownames(dic) <- c("Bernoulli GLM", 
                   "Bernoulli GLM + SRF",
                   "Final selection")
dic

write.csv(dic,"final_dic.csv", row.names = FALSE)

#______________________________________________________________________________________________________________________________________________________________
# 8. Model prediction
#______________________________________________________________________________________________________________________________________________________________

# Import prediction points
pp_studyarea <- read.csv("data/pp_studyarea.csv")
pp_studyarea$btemp_mi <- as.numeric(pp_studyarea$btemp_mi)
pp_studyarea$bath <- as.numeric(pp_studyarea$bath)

pp_studyarea$current <- pp_studyarea$current_mi
pp_studyarea$temp <- pp_studyarea$btemp_mi


# Standardise variables
pp_studyarea$dcoast.std       <- MyStd(pp_studyarea$dcoast)
pp_studyarea$temp.std     <- MyStd(pp_studyarea$btemp_mi)
pp_studyarea$current.std    <- MyStd(pp_studyarea$current_mi)
pp_studyarea$pp.std        <- MyStd(pp_studyarea$pp)
pp_studyarea$temp45.std     <- MyStd(pp_studyarea$temp_45)
pp_studyarea$current45.std    <- MyStd(pp_studyarea$current_45)
pp_studyarea$temp85.std     <- MyStd(pp_studyarea$temp_85)
pp_studyarea$current85.std    <- MyStd(pp_studyarea$current_85)


# xkm and ykm

LL <- LongLatToUTM(x =  pp_studyarea$lon, 
                   y = pp_studyarea$lat, 
                   zone = 29,
                   Hemisphere = "north")
pp_studyarea$xkm <- (LL$X)/1000
pp_studyarea$ykm <- (LL$Y)/1000


#_______________________________________________________________________________
#_______________________________________________________________________________

#  Define the A matrix
Locp <- cbind(pp_studyarea$xkm, pp_studyarea$ykm)
dim(Locp)

#remake the A matrix for prediction
Aprediction <- inla.spde.make.A(mesh = mesh, loc = Locp);
dim(Aprediction)


# Present day prediction stack
pred <- inla.stack(data = list(y = NA), 
                  A = list(Aprediction, 1),                 
                  effects = list(w=1:spde$n.spde,
                                 list(Intercept = 1,
                                      dcoast.std    = pp_studyarea$dcoast.std,
                                      pp.std        = pp_studyarea$pp.std,
                                      current.std   = pp_studyarea$current.std,
                                      temp.std      = pp_studyarea$temp.std
                                      )),
                  tag = 'pred')



# Join the prediction stack with the one for the full data
stk <- inla.stack(est, pred)

final

# Rerun the final model with the empty prediction stack to generate the response variable.
predp<-inla(final, data=inla.stack.data(stk, spde=spde),
                 family= 'binomial', quantiles = NULL,
                 control.predictor=list(link = 1, A=inla.stack.A(stk),compute=FALSE),  
                 control.compute = list(config = TRUE), 
                 control.inla(strategy = 'simplified.laplace', huge = TRUE),
                 verbose = FALSE) 


summary(predp)

# Extracting Predicted Values
index.pred<-inla.stack.index(stk, "pred")$data
post.mean.pred.logit<-predp$summary.linear.predictor[index.pred,"mean"]
p.pred<-exp(post.mean.pred.logit)/(1 + exp(post.mean.pred.logit))

# Rename vector of Predicted values 
Mean_response <- p.pred


# Create df of predicted values and locations 
# Locp - A matrix
# p.pred - P vector

pred_df <- make_prediction_map(Locp, p.pred)
pred_df
