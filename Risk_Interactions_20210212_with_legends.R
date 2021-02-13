##==============================================================================
##
## Script creates conceptual graphs to show interactions among flood risk hazard, 
## vulnerability and exposure
##
## Author: Iman Hosseini-Shakib (ishakib@gmail.com) 
##       
##==============================================================================
## Copyright 2021 Iman Hosseini-Shakib
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
wd=getwd()
setwd(wd)

# Packages Used
#install.packages("ggplot")
#install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

# Functions
# Normal distribution function for hazard PDFs
norm <- function(WL,mu,stdev) {
  (1/(stdev*sqrt(2*pi))*exp((-1/2)*((WL-mu)/stdev)^2))
} 
# A logistic function to mimic the S-shaped depth-damage function
logistic <- function(WL,k,a){
  (1/(1+exp(-k*WL)))^a
} 


# Parameters
# Hazard
WL=1:1000   #hypothetical sample of water levels
WL_PDF1=norm(WL[450],450,65) #hypothetical best guess stationary water levels
WL_PDF2=norm(WL,450,65) #hypothetical distribution of water levels - stationary with uncertainty
WL_PDF3=norm(WL,650,75) #hypothetical distribution of water levels - non-stationary with uncertainty

# Vulnerability
DMG1=logistic((0.020*WL)-10,2,0.2) #hypothetical relative damages - with levee effect
DMG2=logistic((0.015*WL)-10,2,0.4) #hypothetical relative damages - without levee effect

# Exposure
EXP1=WL_PDF1*DMG2[WL[450]] # hypothetical risk exposure with stationary best guess hazard and depth-damage function without levee effect (WL_PDF1 and DMG2)
EXP2=WL_PDF3*DMG1 # hypothetical risk exposure pdf with non-stationary with uncertainty pdf and depth-damage function with levee effect (WL_PDF3 and DMG1)
EXP3=WL_PDF3*DMG2 # hypothetical risk exposure pdf with non-stationary with uncertainty pdf and depth-damage function without levee effect (WL_PDF3 and DMG2)
EXP4=WL_PDF2*DMG2 # hypothetical risk exposure pdf with stationary with uncertainty pdf and depth-damage function without levee effect (WL_PDF2 and DMG2)

# Plot a - Hazard

df1=rbind(data.frame(WL,value=WL_PDF1,group="WL_PDF1"),data.frame(WL,value=WL_PDF2,group="WL_PDF2"),data.frame(WL,value=WL_PDF3,group="WL_PDF3"))

a<-ggplot(df1,aes(WL,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  geom_vline(xintercept = WL[450],size=1.5)+ #hypothetical best guess stationary water levels
  xlab("Water Levels")+
  ylab("Probability Density")+
  scale_colour_manual(name = '', 
                      values =c(WL_PDF1='black',WL_PDF3='darkred',WL_PDF2='darkblue'),
                      labels = c('Stationary\nbest guess\n ','Non-stationary\nwith uncertainty\n ', 'Stationary\nwith uncertainty'))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.00613757)) + 
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        legend.text=element_text(size=12),
        legend.position = c(.2,.7),legend.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = grid::arrow()))

# Plot b - Vulnerability

df2=rbind(data.frame(WL,value=DMG1,group="DMG1"),data.frame(WL,value=DMG2,group="DMG2"))

b<-ggplot(df2,aes(WL,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  xlab("Water Levels")+
  ylab("Damages")+
  scale_colour_manual(name = '', 
                      values =c(DMG2='darkblue',DMG1='darkred'),
                      labels = c('With levee effect', 'Without levee effect'))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1000)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) + 
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        legend.text=element_text(size=12),
        legend.position =  c(0.2,0.7),legend.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = grid::arrow()))



# Plot c - Exposure

df3=rbind(data.frame(p=DMG2[WL[450]],value=EXP1,group="EXP1"),data.frame(p=DMG1,value=EXP2,group="EXP2"),data.frame(p=DMG2,value=EXP3,group="EXP3"),data.frame(p=DMG2,value=EXP4,group="EXP4"))

c<-ggplot(df3,aes(p,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  geom_vline(xintercept = DMG2[WL[450]],size=1.5)+ #hypothetical exposure of stationary best guess hazard without levee effect
  xlab("Damages")+
  ylab("Probability Density")+
  scale_colour_manual(name = '', 
                      values =c(EXP1='black',EXP2='darkorange',EXP3='darkred',EXP4='darkblue'),
                      labels = c('Stationary best guess- with levee effect','Non-stationary with uncertainty, with levee effect',
                                 'Non-stationary with uncertainty, without levee effect', "Stationary with uncertainty, without levee effect"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        legend.text=element_text(size=10),
        legend.position =  c(0.60,0.25),legend.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = grid::arrow()))+
  coord_flip()



# Organizing plots

figure <- ggarrange(b,"",c,"","","",a,"","",
                    labels = c("b) Vulnerability","", "c) Exposure","","","", "   a) Hazard","",""),
                    font.label = list(size = 14),
                    vjust =2, hjust = -.75,
                    ncol = 3, nrow = 3,
                    widths=c(1,0.05,1),heights = c(1,0.05,1))
# Saving a JPG and PDF file of the plot
ggsave("Risk_Interactions_with_legends.jpg",width = 11, height = 8.5, units = c("in"),dpi = 1200)
ggsave("Risk_Interactions_with_legends.pdf",width = 11, height = 8.5, units = c("in"),dpi = 1200)

rm(list=ls())
dev.off()
