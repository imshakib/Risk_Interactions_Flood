##==============================================================================
##
## Script creates conceptual graphs to show interactions among flood risk hazard, 
## Exposure and risk
##
## Authors: Iman Hosseini-Shakib (ikh5084@psu.com)
##          Klaus Keller (kzk10@psu.edu)
## Recoded by: Vivek Srikrishnan (vs498@cornell.edu)
##       
##==============================================================================
## Copyright 2021 Iman Hosseini-Shakib and Klaus Keller
## This file is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, see <http://www.gnu.org/licenses/>.
##==============================================================================

rm(list=ls())
dev.off()

# Packages used
library(VGAM)
library(ggplot2)
library(ggpubr)
library(ggExtra)

nsamp <- 1e6 # how many samples per distribution?

# A logistic function to mimic the S-shaped depth-damage curve with water levels (WL) and coefficients a and k
logistic <- function(WL,k,a){
  (1/(1+exp(-k*WL)))^a
} 

# Water level distribution characteristics
WL=1:1000   #hypothetical water levels
meanWL=525 #hypothetical mean water level for the stationary pdf
sdWL=65 #hypothetical standard deviation of the stationary pdf
meanWL_nonS=550 #hypothetical mean water level for the non-stationary pdf
sdWL_nonS=150 #hypothetical standard deviation of the non-stationary pdf
shapeWL_nonS=5 #hypothetical shape factor of the non-stationary pdf

# Hazard
WLbest=WL[meanWL] #hypothetical best guess stationary water level
# obtain samples from distributions
wl_stat_samp <- rnorm(nsamp,meanWL,sdWL) #hypothetical distribution of water levels - stationary with uncertainty
wl_nonstat_samp <- rskewnorm(nsamp,location=meanWL_nonS,scale=sdWL_nonS,shape=shapeWL_nonS) #hypothetical distribution of water levels - non-stationary with uncertainty

# Vulnerability
dmg_nolevee <- logistic((0.015*WL)-11,0.8,0.6) #hypothetical damages - without levee effect
LEV=1.3 #hypothetical levee effect coefficient
dmg_levee <- dmg_nolevee * LEV #hypothetical damages - with levee effect 

# Risk
rsk_nounc_nolevee <- dmg_nolevee[WLbest] # hypothetical risk with stationary best guess hazard and depth-damage function without levee effect 
rsk_stat_nolevee <- logistic((0.015*wl_stat_samp)-11, 0.8, 0.6) # hypothetical risk pdf with stationary with uncertainty pdf and depth-damage function without levee effect
rsk_nonstat_nolevee <- logistic((0.015*wl_nonstat_samp)-11, 0.8, 0.6) # hypothetical risk pdf with non-stationary with uncertainty pdf and depth-damage function without levee effect
rsk_nonstat_levee <- logistic((0.015*wl_nonstat_samp)-11, 0.8, 0.6) * LEV# hypothetical risk pdf with non-stationary with uncertainty pdf and levee effect

dat_stat_nolevee <- data.frame(wl=wl_stat_samp, dmg=rsk_stat_nolevee, group='stat_nolevee')
dat_nonstat_nolevee <- data.frame(wl=wl_nonstat_samp, dmg=rsk_nonstat_nolevee, group='nonstat_nolevee')
dat_nonstat_levee <- data.frame(wl=wl_nonstat_samp, dmg=rsk_nonstat_levee, group='nonstat_levee')
dat_all <- rbind(dat_stat_nolevee, dat_nonstat_nolevee, dat_nonstat_levee)

dat_dmg_nolevee <- data.frame(wl=WL, dmg=dmg_nolevee[WL], group="nolevee")
dat_dmg_levee <- data.frame(wl=WL, dmg=dmg_levee[WL], group="levee")
dat_dmg_all <- rbind(dat_dmg_nolevee, dat_dmg_levee)
  
# diagnostic plot
dat_dmg <- data.frame(wl=WL, dmg=dmg_nolevee[WL])
p <- ggplot() + geom_line(dat=dat_dmg, aes(x=wl, y=dmg)) + 
  geom_point(dat=dat_stat_nolevee, aes(x=wl, y=dmg), col='red') +
  geom_vline(aes(xintercept=WLbest), linetype='dashed') +
  geom_hline(aes(yintercept=rsk_nounc_nolevee), linetype='dashed', col='red') +
  theme_classic() +
  scale_x_continuous(limits=c(0, 1000), expand=c(0, 0)) + 
  scale_y_continuous(expand=c(0, 0))
p1 <- ggMarginal(p, type='histogram', xparams=list(bins=49), yparams=list(bins=49, fill='red'))

# rebuild original plot
# hazard (water levels)
dens_wl <- density(wl_stat_samp)
a<-ggplot(dat_all[dat_all$group %in% c('stat_nolevee', 'nonstat_nolevee'),], aes(x=wl,group=group)) +
  geom_density(aes(color=group),size=1.5)+
  geom_segment(aes(x = WLbest, xend=WLbest, y=0, yend=max(dens_wl$y)), size=1, linetype="solid", color="black") + #hypothetical best guess stationary water levels
  scale_colour_manual(name = '', 
                      values =c(nonstat_nolevee='darkblue',stat_nolevee='cornflowerblue'))+
  scale_x_continuous("Water Levels", expand = c(0, 0), limits=c(0, 1100)) + 
  scale_y_continuous("Probability Density", expand = c(0, 0)) + 
  annotate(x=580,y=0.006, geom="text",
            label="Neglecting\nclimate change\nand uncertainty",
            color="black",size=6,lineheight = .75,hjust="left")+
  annotate(x=725,y=0.004,geom="text",
            label="Considering\nclimate change\nand uncertainty",
            color="darkblue",size=6,lineheight = .75,hjust="left")+
  annotate(x=450,y=0.0055,geom="text",
            label="Neglecting\nclimate change\nconsidering\nuncertainty", 
            color="cornflowerblue",size=6,lineheight = .75,hjust="right")+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        legend.text=element_text(size=18),
        legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = arrow(angle = 15, length = unit(0.2, "inches"),type = "closed"))) +
  coord_cartesian(ylim=c(0, 0.007))

# Plot b - Vulnerability
b<-ggplot(dat_dmg_all,aes(x=wl, y=dmg, group=group))+
  geom_line(aes(color=group),size=1.5)+
  scale_colour_manual(name = '', 
                      values =c(nolevee='darkkhaki',levee='darkgreen'))+
  scale_x_continuous("Water Levels", expand = c(0, 0), limits = c(0, 1100)) + 
  scale_y_continuous("Damages", expand = c(0, 0), limits = c(0, 1.1*max(dat_dmg_levee$dmg))) + 
  annotate(x=525,y=0.6*max(dat_dmg_levee$dmg), geom="text",
            label="Considering\nlevee effect", color="darkgreen",
            size=6,lineheight = .75)+
  annotate(x=775,y=0.25*max(dat_dmg_levee$dmg), geom="text",
            label="Neglecting\nlevee effect", color="darkkhaki",
            size=6,lineheight = .75)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        legend.text=element_text(size=18),
        legend.position =  "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = arrow(angle = 15, length = unit(0.2, "inches"),type = "closed")))

# Plot c - Risk
c<-ggplot(dat_all,aes(x=dmg, group=group))+
  geom_density(aes(color=group),size=1.5)+
  geom_segment(aes(x=rsk_nounc_nolevee, xend=rsk_nounc_nolevee, y=0, yend=9),size=1,linetype="solid", color="black")+ #hypothetical best guess stationary water levels
  annotate(x=rsk_nounc_nolevee, y=9, geom="point",size=4,color="black")+
  annotate(x=mean(rsk_nonstat_levee),y=9, geom="point",size=4,color="darkred")+
  annotate(x=mean(rsk_nonstat_nolevee),y=9, geom="point",size=4,color="orangered1")+
  annotate(x=mean(rsk_stat_nolevee),y=9, geom="point",size=4,color="orange")+
  scale_colour_manual(name = '', 
                      values =c(nonstat_levee='darkred',nonstat_nolevee='orangered1',stat_nolevee='orange')) +
  scale_x_continuous("Damages", expand = c(0, 0), limits = c(0, 1.4)) + 
  scale_y_continuous("Probability Density", expand = c(0, 0), limits = c(0, 9.5)) + 
  geom_segment(x=0.05,y=8.5,xend = 0.18,yend = 9,
               size=0.5,arrow = arrow(angle = 15, length = unit(0.1, "inches")))+  
  annotate(x=0.1,y=7.5, geom="label", label="Expected\nvalues",
             size=6,hjust="center",lineheight = .75,fill="lightgray")+
  annotate(x=0.28,y=7.5, geom="text",
            label="Neglecting all",
            color="black",size=6,hjust="right",lineheight = .75)+
  annotate(x=1.2,y=0.9, geom="text",
            label="Considering all",
            hjust="left",color="darkred",size=6,lineheight = .75)+
  geom_segment(x=1.2,y=0.85,xend = 1.2,yend = 0.5,
               size=0.5,color="darkred",arrow = arrow(angle = 15, length = unit(0.1, "inches")))+
  annotate(x=0.9,y=1.5, geom="text",
            label="Neglecting levee effect,\nconsidering climate change\nand uncertainty", 
            hjust="left",color="orangered1",size=6,lineheight = .75)+
  geom_segment(x=0.95,y=1.45,xend = 0.95,yend = 0.48,
               size=0.5,color="orangered1",arrow = arrow(angle = 15, length = unit(0.1, "inches")))+
  annotate(x=0.5,y=2.61, geom="text",
            label="Neglecting climate change\nand levee effect,\nconsidering uncertainty", 
            hjust="left",color="orange",size=6,lineheight = .75)+
  geom_segment(x=0.37,y=3.1,xend = 0.29,yend = 3.1,
               size=0.5,color="orange",arrow = arrow(angle = 15, length = unit(0.1, "inches")))+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        legend.text=element_text(size=18),
        legend.position =  "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = arrow(angle = 15, length = unit(0.2, "inches"),
                                               type = "closed")))+
  coord_flip()

# Organizing plots
figure1 <- ggarrange(b,c,a,
                     labels = c(" b) Exposure","   c) Risk","  a) Hazard"),
                     font.label = list(size = 18),
                     vjust =1.5, hjust =-.3,
                     ncol = 2, nrow = 2) 

# Coordinates of the start and end points of the leader line
x1=0.254; y1=0.40
x2=0.53; y2=0.615
# Adding leader lines and arrowheads 
figure<-figure1+geom_segment(aes(x=x1,xend=x1,y=y1,yend=y2),size=1,linetype="dotted")+
  geom_segment(aes(x=x1,xend=x1,y=y2-0.01,yend=y2),arrow = arrow(angle = 15, length = unit(0.2, "inches"),type="closed"))+
  geom_segment(aes(x=x1,xend=x2,y=y2,yend=y2),size=1,linetype="dotted")+
  geom_segment(aes(x=x2-0.01,xend=x2,y=y2,yend=y2),arrow = arrow(angle = 15, length = unit(0.2, "inches"),type="closed"))

# Saving a JPG and PDF file of the plot
ggsave("Risk_Interactions_20210228.jpg",width = 11, height = 8.5, units = c("in"),dpi = 600)
ggsave("Risk_Interactions_20210228.pdf",width = 11, height = 8.5, units = c("in"),dpi = 600)

# Time to clean up!
rm(list=ls())
