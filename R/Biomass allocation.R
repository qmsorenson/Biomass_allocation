setwd("C:/Users/Quinn Sorenson/Google Drive/Graduate School/Research/Remnant/Biomass allocation/Data/Biomass allocation data files from R")
library(lme4)
library(Hmisc)
library(plyr)
library(lsmeans)
library(ggplot2)
library(lmerTest)
library(grid)
library(gridExtra)

com <- read.csv("EUPCOM.csv")
cun <- read.csv("EUPCUN.csv")
odo <- read.csv("SOLODO.csv")
hyp <- read.csv("HYPHYP.csv")
gra <- read.csv("PITGRA.csv")
tri <- read.csv("SCLTRI.csv")

fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
  }


###########################################################
##################      ROOT      #########################
###########################################################



### EUPCOM ###
mrlm <- lmer(RMF ~ LUH*CT + biomass + (1|Site), data=com)
anova(mrlm)
lsrcom <- lsmeansLT(mrlm)
difflsmeans(mrlm)
mrlm2 <- lmer(RMF ~ LUH*CT + biomass + (1|Site), data=com)
anova(mrlm)
anova(mrlm2)
anova(mrlm, mrlm2)

### EUPCUN ###
nrlm <- lmer(RMF ~ LUH*CT + (1|Site), data=cun)
anova(nrlm)
summary(nrlm)
lsrcun <- lsmeansLT(nrlm)
difflsmeans(nrlm)

### SOLODO ###
orlm <- lmer(RMF ~ LUH*CT  + (1|Site), data=odo)
anova(orlm)
summary(orlm)
lsrodo <- lsmeansLT(orlm)
difflsmeans(orlm)

### HYPHYP ###
prlm <- lmer(RMF ~ LUH*CT + (1|Site), data=hyp)
anova(prlm)
summary(prlm)
lsrhyp <- lsmeansLT(prlm)
difflsmeans(prlm)

### PITGRA ###
arlm <- lmer(RMF ~ LUH*CT + (1|Site), data=gra)
anova(arlm)
summary(arlm)
lsrgra <- lsmeansLT(arlm)
difflsmeans(arlm)

### SCLTRI ###
irlm <- lmer(RMF ~ LUH*CT  + biomass + (1|Site), data=tri)
anova(irlm)
summary(irlm)
lsrtri <- lsmeansLT(irlm)
difflsmeans(irlm)

####COM###

p1 <- ggplot(data = lsrcom$lsmeans.table[complete.cases(lsrcom$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsrcom$lsmeans.table[complete.cases(lsrcom$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsrcom$lsmeans.table[complete.cases(lsrcom$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("a)", italic(" Eupatorium compositifolium"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +
  scale_y_continuous(labels = fmt_dcimals(2)) +
  scale_colour_manual(values=c("khaki3", "khaki4")) +
  scale_shape_manual(values=c(79, 19)) +
  scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black")) legend.position="none")

####CUN####

p2 <- ggplot(data = lsrcun$lsmeans.table[complete.cases(lsrcun$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsrcun$lsmeans.table[complete.cases(lsrcun$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsrcun$lsmeans.table[complete.cases(lsrcun$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("b)", italic(" Eupatorium cuneifolium"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("khaki3", "khaki4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### ODO ####
p3<- ggplot(data = lsrodo$lsmeans.table[complete.cases(lsrodo$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsrodo$lsmeans.table[complete.cases(lsrodo$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsrodo$lsmeans.table[complete.cases(lsrodo$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("c)", italic(" Solidago odora"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("khaki3", "khaki4")) +   
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### HYP ####
p4 <- ggplot(data = lsrhyp$lsmeans.table[complete.cases(lsrhyp$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsrhyp$lsmeans.table[complete.cases(lsrhyp$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsrhyp$lsmeans.table[complete.cases(lsrhyp$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("d)", italic(" Hypericum hypericoides"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("khaki3", "khaki4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### GRA ####
p5<- ggplot(data = lsrgra$lsmeans.table[complete.cases(lsrgra$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsrgra$lsmeans.table[complete.cases(lsrgra$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsrgra$lsmeans.table[complete.cases(lsrgra$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("e)", italic(" Pityopsis graminifolia"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("khaki3", "khaki4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### TRI ####
p6 <- ggplot(data = lsrtri$lsmeans.table[complete.cases(lsrtri$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsrtri$lsmeans.table[complete.cases(lsrtri$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsrtri$lsmeans.table[complete.cases(lsrtri$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("f)", italic(" Scleria triglomerata"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("khaki3", "khaki4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")


grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, bottom = textGrob("Land-use history", gp=gpar(fontsize=14)), left = textGrob("Mean root mass fraction g/g", gp=gpar(fontsize=14), rot = 90))


###########################################################
##################      STEM      #########################
###########################################################



### EUPCOM ###
mslm <- lmer(SMF ~ LUH*CT + (1|Site), data=com)
anova(mslm)
lsscom <- lsmeansLT(mslm)
difflsmeans(mslm)
mslm2 <- lmer(SMF ~ LUH*CT + biomass + (1|Site), data=com)
anova(mslm)
anova(mslm2)
anova(mslm, mslm2)

### EUPCUN ###
nslm <- lmer(SMF ~  LUH*CT + biomass + (1|Site), data=cun)
anova(nslm)
summary(nslm)
lsscun <- lsmeansLT(nslm)
difflsmeans(nslm)

### SOLODO ###
oslm <- lmer(SMF ~ LUH*CT +  (1|Site), data=odo)
anova(oslm)
summary(oslm)
lssodo <- lsmeansLT(oslm)
difflsmeans(oslm)

### HYPHYP ###
pslm <- lmer(SMF ~ LUH*CT + (1|Site), data=hyp)
anova(pslm)
summary(pslm)
lsshyp <- lsmeansLT(pslm)
difflsmeans(pslm)

### PITGRA ###
aslm <- lmer(SMF ~ LUH*CT + (1|Site), data=gra)
anova(aslm)
summary(aslm)
lssgra <- lsmeansLT(aslm)
difflsmeans(aslm)

### SCLTRI ###
islm <- lmer(SMF ~ LUH*CT  + biomass + (1|Site), data=tri)
anova(islm)
summary(islm)
lsstri <- lsmeansLT(islm)
difflsmeans(islm)

####COM###

p7 <- ggplot(data = lsscom$lsmeans.table[complete.cases(lsscom$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsscom$lsmeans.table[complete.cases(lsscom$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsscom$lsmeans.table[complete.cases(lsscom$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("a)", italic(" Eupatorium compositifolium"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkgoldenrod", "darkgoldenrod4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

####CUN####

p8 <- ggplot(data = lsscun$lsmeans.table[complete.cases(lsscun$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsscun$lsmeans.table[complete.cases(lsscun$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsscun$lsmeans.table[complete.cases(lsscun$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("b)", italic(" Eupatorium cuneifolium"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkgoldenrod", "darkgoldenrod4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### ODO ####
p9<- ggplot(data = lssodo$lsmeans.table[complete.cases(lssodo$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lssodo$lsmeans.table[complete.cases(lssodo$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lssodo$lsmeans.table[complete.cases(lssodo$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("c)", italic(" Solidago odora"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkgoldenrod", "darkgoldenrod4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### HYP ####
p10 <- ggplot(data = lsshyp$lsmeans.table[complete.cases(lsshyp$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsshyp$lsmeans.table[complete.cases(lsshyp$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsshyp$lsmeans.table[complete.cases(lsshyp$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("d)", italic(" Hypericum hypericoides"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkgoldenrod", "darkgoldenrod4")) +   
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### GRA ####
p11<- ggplot(data = lssgra$lsmeans.table[complete.cases(lssgra$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lssgra$lsmeans.table[complete.cases(lssgra$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lssgra$lsmeans.table[complete.cases(lssgra$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("e)", italic(" Pityopsis graminifolia"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkgoldenrod", "darkgoldenrod4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### TRI ####
p12 <- ggplot(data = lsstri$lsmeans.table[complete.cases(lsstri$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsstri$lsmeans.table[complete.cases(lsstri$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsstri$lsmeans.table[complete.cases(lsstri$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("f)", italic(" Scleria triglomerata"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkgoldenrod", "darkgoldenrod4")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")


grid.arrange(p7, p8, p9, p10, p11, p12, ncol = 3, bottom = textGrob("Land-use history", gp=gpar(fontsize=14)), left = textGrob("Mean stem mass fraction g/g", gp=gpar(fontsize=14), rot = 90))

###########################################################
##################      LEAF      #########################
###########################################################



### EUPCOM ###
mllm <- lmer(LMF ~ LUH*CT + biomass + (1|Site), data=com)
anova(mllm)
lsfcom <- lsmeansLT(mllm)
difflsmeans(mllm)
mllm2 <- lmer(LMF ~ LUH*CT + biomass + (1|Site), data=com)
anova(mllm)
anova(mllm2)
anova(mllm, mllm2)

### EUPCUN ###
nllm <- lmer(LMF ~ LUH*CT + (1|Site), data=cun)
anova(nllm)
summary(nllm)
lsfcun <- lsmeansLT(nllm)
difflsmeans(nllm)

### SOLODO ###
ollm <- lmer(LMF ~ LUH*CT + (1|Site), data=odo)
anova(ollm)
summary(ollm)
lsfodo <- lsmeansLT(ollm)
difflsmeans(ollm)

### HYPHYP ###
pllm <- lmer(LMF ~ LUH*CT + (1|Site), data=hyp)
anova(pllm)
summary(pllm)
lsfhyp <- lsmeansLT(pllm)
difflsmeans(pllm)

### PITGRA ###
allm <- lmer(LMF ~ LUH*CT + (1|Site), data=gra)
anova(allm)
summary(allm)
lsfgra <- lsmeansLT(allm)
difflsmeans(allm)

### SCLTRI ###
illm <- lmer(LMF ~ LUH*CT  + biomass + (1|Site), data=tri)
anova(illm)
summary(illm)
lsftri <- lsmeansLT(illm)
difflsmeans(illm)

####COM###

p13 <- ggplot(data = lsfcom$lsmeans.table[complete.cases(lsfcom$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsfcom$lsmeans.table[complete.cases(lsfcom$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsfcom$lsmeans.table[complete.cases(lsfcom$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("a)", italic(" Eupatorium compositifolium"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkolivegreen3", "darkolivegreen")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

####CUN####

p14 <- ggplot(data = lsfcun$lsmeans.table[complete.cases(lsfcun$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsfcun$lsmeans.table[complete.cases(lsfcun$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsfcun$lsmeans.table[complete.cases(lsfcun$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("b)", italic(" Eupatorium cuneifolium"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkolivegreen3", "darkolivegreen")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### ODO ####
p15 <- ggplot(data = lsfodo$lsmeans.table[complete.cases(lsfodo$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsfodo$lsmeans.table[complete.cases(lsfodo$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsfodo$lsmeans.table[complete.cases(lsfodo$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("c)", italic(" Solidago odora"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkolivegreen3", "darkolivegreen")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### HYP ####
p16 <- ggplot(data = lsfhyp$lsmeans.table[complete.cases(lsfhyp$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsfhyp$lsmeans.table[complete.cases(lsfhyp$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsfhyp$lsmeans.table[complete.cases(lsfhyp$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("d)", italic(" Hypericum hypericoides"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkolivegreen3", "darkolivegreen")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### GRA ####
p17 <- ggplot(data = lsfgra$lsmeans.table[complete.cases(lsfgra$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsfgra$lsmeans.table[complete.cases(lsfgra$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsfgra$lsmeans.table[complete.cases(lsfgra$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("e)", italic(" Pityopsis graminifolia"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkolivegreen3", "darkolivegreen")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")

#### TRI ####
p18 <- ggplot(data = lsftri$lsmeans.table[complete.cases(lsftri$lsmeans.table[,1:2]),], aes(x = factor(LUH), y = Estimate, colour = CT, shape = CT)) + 
  geom_point(data = lsftri$lsmeans.table[complete.cases(lsftri$lsmeans.table[,1:2]),], aes(y = Estimate), size = 6, show.legend = FALSE) +
  geom_line(data = lsftri$lsmeans.table[complete.cases(lsftri$lsmeans.table[,1:2]),], aes(y = Estimate, group = CT, linetype = CT), show.legend = FALSE, size = 1.7 ) + 
  geom_errorbar(aes(ymax = Estimate+`Standard Error`, ymin=Estimate-`Standard Error`, width = 0.1)) + 
  labs(title = expression(paste("f)", italic(" Scleria triglomerata"))), x = NULL, y = NULL, shape = "Tree density", 
       linetype = "Tree density") + 
  scale_x_discrete(labels = c("M" = "Post-ag", "R" = "Remnant")) +  scale_shape_manual(values=c(79, 19)) + scale_linetype_manual(values = c("dashed", "solid"), name = "Legend") + scale_colour_manual(values=c("darkolivegreen3", "darkolivegreen")) +   
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), aspect.ratio = .9,
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 10), axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"), legend.position="none")


grid.arrange(p13, p14, p15, p16, p17, p18, ncol = 3, bottom = textGrob("Land-use history", gp=gpar(fontsize=14)), left = textGrob("Mean leaf mass fraction g/g", gp=gpar(fontsize=14), rot = 90))

###########################################################
##################      SSL      ##########################
###########################################################



### EUPCOM ###
Rlm <- lmer(SSL ~ CT*biomass + (1|Site), data=com)
anova(Rlm)
anova(Rlm)

### EUPCuM ###
Rlm <- lmer(SSL ~ LUH*CT + biomass +  (1|Site), data=cun)
anova(Rlm)
lsmeansLT(Rlm)
difflsmeans(Rlm)
summary(Rlm)
### SOLODO ###
Rlm <- lmer(SSL ~ LUH*CT + biomass + (1|Site), data=odo)
anova(Rlm)
summary(Rlm)


library(ggplot2)
library(plyr)

####COM###

toothInt <- ddply(com2,.(LUH,CT),summarise, val = mean(SSL))

ggplot(com2, aes(x = factor(LUH), y = SSL, colour = CT)) + 
  geom_boxplot(size = 1.5) +
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw() 
  
  ####CUN###
  
  toothInt <- ddply(cun2,.(LUH,CT),summarise, val = mean(SSL))

ggplot(cun2, aes(x = factor(LUH), y = SSL, colour = CT)) + 
  geom_boxplot(size = 1.5) + 
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw()

####ODO###

toothInt <- ddply(odo2,.(LUH,CT),summarise, val = mean(SSL))

ggplot(odo2, aes(x = factor(LUH), y = SSL, colour = CT)) + 
  geom_boxplot(size = 1.5) + 
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw()

##############################################################################################
####################################### CODE TRASH BIN #######################################
##############################################################################################


####CUN###

toothInt <- ddply(cun2,.(LUH,CT),summarise, val = mean(RMF))

ggplot(cun2, aes(x = factor(LUH), y = RMF, colour = CT)) + 
  geom_boxplot(size = 1.5) + 
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw()

####ODO###

toothInt <- ddply(odo2,.(LUH,CT),summarise, val = mean(RMF))

ggplot(odo2, aes(x = factor(LUH), y = RMF, colour = CT)) + 
  geom_boxplot(size = 1.5) + 
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw()

###########################################################
##################      STEM      #########################
###########################################################


library(lmerTest)
### EUPCOM ###
Rlm <- lmer(SMF ~ LUH*CT + (1|Site), data=com)
anova(Rlm)
Rlm2 <- lmer(SMF ~ LUH*CT + Total_Plant_Biomass + (1|Site), data=com)
anova(Rlm)
anova(Rlm2)
anova(Rlm, Rlm2)

### EUPCuM ###
Rlm <- lmer(SMF ~ LUH*CT + (1|Site), data=cun)
anova(Rlm)
summary(Rlm)
### SOLODO ###
Rlm <- lmer(SMF ~ LUH*CT + (1|Site), data=odo)
anova(Rlm)
summary(Rlm)


library(ggplot2)
library(plyr)

####COM###

toothInt <- ddply(com2,.(LUH,CT),summarise, val = mean(SMF))

ggplot(com2, aes(x = factor(LUH), y = SMF, colour = CT)) + 
  geom_boxplot(size = 1.5) +
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw() 

####CUN###

toothInt <- ddply(cun2,.(LUH,CT),summarise, val = mean(SMF))

ggplot(cun2, aes(x = factor(LUH), y = SMF, colour = CT)) + 
  geom_boxplot(size = 1.5) + 
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw()

####ODO###

toothInt <- ddply(odo2,.(LUH,CT),summarise, val = mean(SMF))

ggplot(odo2, aes(x = factor(LUH), y = SMF, colour = CT)) + 
  geom_boxplot(size = 1.5) + 
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw()



###########################################################
##################      LEAF      #########################
###########################################################


library(lmerTest)
### EUPCOM ###
Rlm <- lmer(LMF ~ LUH*CT + (1|Site), data=com)
anova(Rlm)
Rlm2 <- lmer(LMF ~ LUH*CT + Total_Plant_Biomass + (1|Site), data=com)
anova(Rlm)
anova(Rlm2)
anova(Rlm, Rlm2)

### EUPCuM ###
Rlm <- lmer(LMF ~ LUH*CT + (1|Site), data=cun)
anova(Rlm)
summary(Rlm)
### SOLODO ###
Rlm <- lmer(LMF ~ LUH*CT + (1|Site), data=odo)
anova(Rlm)
summary(Rlm)


library(ggplot2)
library(plyr)

####COM###

toothInt <- ddply(com2,.(LUH,CT),summarise, val = mean(LMF))

ggplot(com2, aes(x = factor(LUH), y = LMF, colour = CT)) + 
  geom_boxplot(size = 1.5) +
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw() 

####CUN###

toothInt <- ddply(cun2,.(LUH,CT),summarise, val = mean(LMF))

ggplot(cun2, aes(x = factor(LUH), y = LMF, colour = CT)) + 
  geom_boxplot(size = 1.5) + 
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw()

####ODO###

toothInt <- ddply(odo2,.(LUH,CT),summarise, val = mean(LMF))

ggplot(odo2, aes(x = factor(LUH), y = LMF, colour = CT)) + 
  geom_boxplot(size = 1.5) + 
  geom_point(data = toothInt, aes(y = val), size = 8, shape = 17) +
  geom_line(data = toothInt, aes(y = val, group = CT), size = 1.5) + 
  theme_bw()



