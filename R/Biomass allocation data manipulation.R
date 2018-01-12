setwd("C:/Users/Quinn Sorenson/Google Drive/Graduate School/Research/Remnant/Biomass allocation/Data")
library(lme4)
library(Hmisc)
library(plyr)

da15 <- read.csv("BAdata.csv")
da16 <- read.csv("BA2016.csv")
da15 <- plyr::rename(da15, c("Main_Stem_Specific_Length" = "SSL", "Species" = "Spp", "Total_Plant_Biomass" = "biomass"))
da16 <- plyr::rename(da16, c("Land.use.history" = "LUH", "Canopy.thinning" = "CT", "SPP" = "Spp", "Species.number" = "SN", "Roots..g." = "Root_mass", "Stems..g." = "Stem_mass", "Leaves..g." = "Leaf_mass",  "Flowers..g." = "Flower_mass", "Seeds..g." = "Seed_mass", "Total.biomass" = "biomass"))

#2015
com <- da15[da15$Spp == "EUPCOM",]
cun <- da15[da15$Spp == "EUPCUN",]
odo <- da15[da15$Spp == "SOLODO",]

#2016
hyp <- da16[da16$Spp == "HYPHYP",]
gra <- da16[da16$Spp == "PITGRA",]
tri <- da16[da16$Spp == "SCLTRI",]

write.csv(com, "C:/Users/Quinn Sorenson/Google Drive/Graduate School/Research/Remnant/Biomass allocation/Data/Biomass allocation data files from R/EUPCOM.csv")
write.csv(cun, "C:/Users/Quinn Sorenson/Google Drive/Graduate School/Research/Remnant/Biomass allocation/Data/Biomass allocation data files from R/EUPCUN.csv")
write.csv(odo, "C:/Users/Quinn Sorenson/Google Drive/Graduate School/Research/Remnant/Biomass allocation/Data/Biomass allocation data files from R/SOLODO.csv")
write.csv(hyp, "C:/Users/Quinn Sorenson/Google Drive/Graduate School/Research/Remnant/Biomass allocation/Data/Biomass allocation data files from R/HYPHYP.csv")
write.csv(gra, "C:/Users/Quinn Sorenson/Google Drive/Graduate School/Research/Remnant/Biomass allocation/Data/Biomass allocation data files from R/PITGRA.csv")
write.csv(tri, "C:/Users/Quinn Sorenson/Google Drive/Graduate School/Research/Remnant/Biomass allocation/Data/Biomass allocation data files from R/SCLTRI.csv")


hist(com$RMF)
hist(cun$RMF)
hist(odo$RMF)
hist(hyp$RMF)
hist(gra$RMF)
hist(tri$RMF)

summary <- aggregate(cbind(RMF, LMF, SMF) ~ Site + LUH + CT + Spp, data= da15, FUN=length)
summary2 <- aggregate(cbind(RMF, LMF, SMF) ~ LUH + CT + Spp, data= summary, FUN=length)

com2 <- aggregate(cbind(RMF, LMF, SMF, SSL) ~ Site + LUH + CT + Spp, data= com, FUN=mean)
cun2 <- aggregate(cbind(RMF, LMF, SMF, SSL) ~ Site + LUH + CT + Spp, data= cun, FUN=mean)
odo2 <- aggregate(cbind(RMF, LMF, SMF, SSL) ~ Site + LUH + CT + Spp, data= odo, FUN=mean)

hyp2 <- aggregate(cbind(RMF, LMF, SMF) ~ Site + LUH + CT + Spp, data= hyp, FUN=mean)
gra2 <- aggregate(cbind(RMF, LMF, SMF) ~ Site + LUH + CT + Spp, data= gra, FUN=mean)
tri2 <- aggregate(cbind(RMF, LMF, SMF) ~ Site + LUH + CT + Spp, data= tri, FUN=mean)


hist(com2$RMF)
hist(cun2$RMF)
hist(odo2$RMF)

hist(com2$RMF~com2$LUH*com2$CT)
hist(cun2$RMF~cun2$LUH*cun2$CT)
hist(odo2$RMF~odo2$LUH*odo2$CT)

plot(com$biomass, com$RMF)
plot(cun$biomass, cun$RMF)
plot(odo$biomass, odo$RMF)

plot(hyp$biomass, hyp$RMF)
plot(gra$biomass, gra$RMF)
plot(tri$biomass, tri$RMF)

plot(com$SMF, com$SSL)
plot(cun$SMF, cun$SSL)
plot(odo$SMF, odo$SSL)

