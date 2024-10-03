
# Comparing the spread of data

library(rWind)
library(dplyr)
library(ggplot2)
library(egg)
library(ggpubr)

Skatedat <- read.csv("data/data_f_n.csv")
Skatedat$skate_gen <- as.factor(Skatedat$skate_gen)


# Plot boxplots of environmental data

dco <- ggplot (Skatedat, aes(x=, y = dcoast)) +
  geom_boxplot() + facet_wrap(~skate_gen)+ ylab("Distance to coast/°")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank(),   
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
dco

bath <- ggplot (Skatedat, aes(x=, y = bath)) +
  geom_boxplot() + facet_wrap(~skate_gen)+ ylab("Bathymetry/m")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank(),   
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

bath
btemp <- ggplot (Skatedat, aes(x=, y = btemp_mi)) +
  geom_boxplot() + facet_wrap(~skate_gen)+ ylab("Bottom temperature/°C")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank(),   
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
btemp

current <- ggplot (Skatedat, aes(x=, y = current_mi)) +
  geom_boxplot() + facet_wrap(~skate_gen)+ ylab("Current speed/ms-1")+ scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank(),   
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

current

hauldur <- ggplot (Skatedat, aes(x=, y = hauldur)) +
  geom_boxplot() + facet_wrap(~skate_gen)+ ylab("Haul Duration/mins")+ scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank(),   
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

hauldur





ggarrange(dco, bath, btemp, current,
                    ncol = 4, nrow = 1) 
  ggexport(filename = "environmentalplot.jpeg", width = 1218, height = 908)
  

#_______________________________________________________________________________

# plot important variable(s)

Skate$cpue_01 <- as.factor(Skate$cpue_01)
Skate$dcoast_km <- Skate$dcoast/1000

bath <- ggplot (Skatedat, aes(x= skate_gen, y = bath)) +
  geom_jitter(aes(colour = skate_gen), size = 3,  alpha = 0.5, width=0.2)+
  geom_violin(alpha = 0) + ylab("Bathymetry /m") + xlab("")+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  ggtitle("p = 0.005*")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 30, colour = "red", hjust = 1),
        axis.title=element_text(size=30,face="bold"),
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))
bath

btemp <- ggplot (Skatedat, aes(x= skate_gen, y = btemp_mi)) +
  geom_jitter(aes(colour = skate_gen), size = 3,  alpha = 0.5, width=0.2)+
  geom_violin(alpha = 0) + ylab("Bottom Temperature /°C") + xlab("")+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  ggtitle("p = 7.22e-06*")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 30, colour = "red", hjust = 1),
        axis.title=element_text(size=30,face="bold"),
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))
btemp

dcoast <- ggplot (Skatedat, aes(x= skate_gen, y = dcoast)) +
  geom_jitter(aes(colour = skate_gen), size = 3,  alpha = 0.5, width=0.2)+
  geom_violin(alpha = 0) + ylab("Distance to coast/ °") + xlab("")+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  ggtitle("p = 0.086")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 30, hjust = 1), 
        axis.title=element_text(size=30,face="bold"),
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))
dcoast

current <- ggplot (Skatedat, aes(x= skate_gen, y = current_mi)) +
  geom_jitter(aes(colour = skate_gen), size = 3,  alpha = 0.5, width=0.2)+
  geom_violin(alpha = 0) + ylab("Surface current/ ms-1") + xlab("")+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  ggtitle("p = 0.146")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 30, hjust = 1),
        axis.title=element_text(size=30,face="bold"),
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))
current


hauldur <- ggplot (Skatedat, aes(x= skate_gen, y = hauldur)) +
  geom_jitter(aes(colour = skate_gen), size = 3,  alpha = 0.5, width=0.2)+
  geom_violin(alpha = 0) + ylab("Haul Duration/ mins") + xlab("")+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  ggtitle("p = 0.585")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 30, hjust = 1),
        axis.title=element_text(size=30,face="bold"),
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))
hauldur

hauldist <- ggplot (Skatedat, aes(x= skate_gen, y = distance)) +
  geom_jitter(aes(colour = skate_gen), size = 3,  alpha = 0.5, width=0.2)+
  geom_violin(alpha = 0) + ylab("Haul Distance/ m") + xlab("")+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  ggtitle("p = 0.853")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 30, hjust = 1),
        axis.title=element_text(size=30,face="bold"),
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))

hauldist


groundspeed <- ggplot (Skatedat, aes(x= skate_gen, y = groundspeed)) +
  geom_jitter(aes(colour = skate_gen), size = 3,  alpha = 0.5, width=0.2)+
  geom_violin(alpha = 0) + ylab("Groundspeed/ Kn") + xlab("")+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  ggtitle("p = 0.443")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 30, hjust = 1),
        axis.title=element_text(size=30,face="bold"),
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))

groundspeed



ggarrange(bath, btemp, current, dcoast, hauldur, hauldist, groundspeed,
          ncol = 4, nrow = 2) %>%
  ggexport(filename = "environmentalplot.jpeg", width = 2100, height = 1200)
  

###

#dcoast is not normal
ggplot (Skatedat, aes(x= dcoast)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

shapiro.test(Skatedat$dcoast)

wilcox.test(dcoast ~ skate_gen, data = Skatedat,
                   exact = FALSE)  # P = 0.086 NO SIGNIFICANT DIFFERENCE


#btemp is not normal
ggplot (Skatedat, aes(x= btemp_mi)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

shapiro.test(Skatedat$btemp_mi)

wilcox.test(btemp_mi ~ skate_gen, data = Skatedat,
            exact = FALSE)   # P = 7.22e-06 SIGNIFICANT DIFFERENCE

#current is not normal
ggplot (Skatedat, aes(x= current_mi)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

shapiro.test(Skatedat$current_mi)

wilcox.test(current_mi ~ skate_gen, data = Skatedat,
            exact = FALSE)   # P = 0.146 NO SIGNIFICANT DIFFERENCE

#depth is not normal
ggplot (Skatedat, aes(x= bath)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

shapiro.test(Skatedat$bath)

wilcox.test(bath ~ skate_gen, data = Skatedat,
            exact = FALSE)   # P = 0.005 SIGNIFICANT DIFFERENCE

#haul duration is not normal
ggplot (Skatedat, aes(x= hauldur)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

shapiro.test(Skatedat$hauldur)

wilcox.test(hauldur ~ skate_gen, data = Skatedat,
            exact = FALSE) # P = 0.585 NO SIGNIFICANCE

#haul distance is not normal
ggplot (Skatedat, aes(x= distance)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

shapiro.test(Skatedat$distance)

wilcox.test(distance ~ skate_gen, data = Skatedat,
            exact = FALSE) # P = 0.585 NO SIGNIFICANCE

#groundspeed is not normal
ggplot (Skatedat, aes(x= groundspeed)) +
  geom_histogram(bins = 10) + 
  theme_classic() 

shapiro.test(Skatedat$groundspeed)

wilcox.test(groundspeed ~ skate_gen, data = Skatedat,
            exact = FALSE) # P = 0.585 NO SIGNIFICANCE
