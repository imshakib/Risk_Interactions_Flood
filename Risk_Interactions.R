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
WL_PDF1=norm(WL,500,25) #hypothetical distribution of water levels - overconfident
WL_PDF2=norm(WL,350,50) #hypothetical distribution of water levels - deep uncertainty - low
WL_PDF3=norm(WL,650,50) #hypothetical distribution of water levels - deep uncertainty - high

# Vulnerability
Depth=seq(from=-10,by=0.02,length.out = 1000) #hypothetical sample of water levels for depth-damage curve
DMG1=logistic(Depth,2,0.4) #hypothetical relative damages - without levee effect
DMG2=logistic((0.75*Depth),2,0.8) #hypothetical relative damages - with levee effect

# Exposure
EXP1=WL_PDF2*DMG2 # hypothetical risk exposure pdf with deep uncertainty low hazard pdf and depth-damage function with levee effect (WL_PDF2 and DMG2)
EXP2=WL_PDF1*DMG1 # hypothetical risk exposure pdf with overconfident pdf and depth-damage function without levee effect (WL_PDF1 and DMG1)
EXP3=WL_PDF3*DMG2 # hypothetical risk exposure pdf with deep uncertainty high hazard pdf and depth-damage function with levee effect (WL_PDF3 and DMG2)

# Plot a - Hazard

df1=rbind(data.frame(WL,value=WL_PDF1,group="WL_PDF1"),data.frame(WL,value=WL_PDF2,group="WL_PDF2"),data.frame(WL,value=WL_PDF3,group="WL_PDF3"))

a<-ggplot(df1,aes(WL,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  xlab("Water Levels")+
  ylab("Probability Density")+
  scale_colour_manual(name = '', 
                      values =c(WL_PDF1='black',WL_PDF3='red',WL_PDF2='darkgreen'),
                      labels = c('Overconfident','Deep Uncertainty - High', 'Deep Uncertainty - Low'))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = grid::arrow()))

# Plot b - Vulnerability

df2=rbind(data.frame(Depth,value=DMG1,group="DMG1"),data.frame(Depth,value=DMG2,group="DMG2"))

b<-ggplot(df2,aes(Depth,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  xlab("Water Levels")+
  ylab("Damages")+
  scale_colour_manual(name = '', 
                      values =c(DMG2='blue',DMG1='red'),
                      labels = c('With Levee Effect', 'Without Levee Effect'))+
  scale_x_continuous(expand = c(0, 0), limits = c(-10, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) + 
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = grid::arrow()))



# Plot c - Exposure

df3=rbind(data.frame(p=DMG2,value=EXP1,group="EXP1"),data.frame(p=DMG1,value=EXP2,group="EXP2"),data.frame(p=DMG2,value=EXP3,group="EXP3"))

c<-ggplot(df3,aes(p,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  xlab("Damages")+
  ylab("Probability Density")+
  scale_colour_manual(name = '', 
                      values =c(EXP1='blue',EXP2='black',EXP3='red'),
                      labels = c('Deep Uncertainty - Low, With levee effect','Overconfident, No levee effect', 'Deep uncertainty - High, With levee effect'))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = grid::arrow()))+
  coord_flip()

# Graph Legend
d<-ggplot()+
  geom_text(aes(x=1,y=5),label="a) Hazard",fontface="bold")+
  geom_text(aes(x=10,y=5),label="")+
  geom_point(aes(x=0,y=4.5),shape=15, size =5, color="black")+
  geom_text(aes(x=2,y=4.5),label="Overconfident")+
  geom_point(aes(x=0,y=4),shape=15, size =5, color="red")+
  geom_text(aes(x=2,y=4),label="Deep Uncertainty - High", hjust=0.3)+
  geom_point(aes(x=0,y=3.5),shape=15, size =5, color="darkgreen")+
  geom_text(aes(x=2,y=3.5),label="Deep Uncertainty - Low", hjust= 0.3)+
  geom_text(aes(x=1,y=3),label="b) Vulnerability",hjust=.3,fontface="bold")+
  geom_point(aes(x=0,y=2.5),shape=15, size =5, color="blue")+
  geom_text(aes(x=2,y=2.5),label="With Levee Effect",hjust=0.4)+
  geom_point(aes(x=0,y=2),shape=15, size =5, color="red")+
  geom_text(aes(x=2,y=2),label="Without Levee Effect",hjust=0.35)+
  geom_text(aes(x=1,y=1.5),label="c) Damages",hjust=.35,fontface="bold")+
  geom_point(aes(x=0,y=1),shape=15, size =5, color="blue")+
  geom_text(aes(x=2,y=1),label="Deep Uncertainty - Low, With levee effect",hjust=0.18)+
  geom_point(aes(x=0,y=0.5),shape=15, size =5, color="black")+
  geom_text(aes(x=2,y=0.5),label="Overconfident, No levee effect",hjust=0.25)+
  geom_point(aes(x=0,y=0),shape=15, size =5, color="red")+
  geom_text(aes(x=2,y=0),label="Deep uncertainty - High, With levee effect",hjust=0.18)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_blank()) 

# Organizing plots

figure <- ggarrange(b,c,a,d,
                    labels = c("b) Vulnerability", "   c) Damages", "   a) Hazard",""),
                    font.label = list(size = 12),
                    vjust =2, hjust = -.75,
                    ncol = 2, nrow = 2)
# Saving a JPG and PDF file of the plot
ggsave("Risk_Interactions.jpg",width = 11, height = 12, units = c("in"),dpi = 1200)
ggsave("Risk_Interactions.pdf",width = 11, height = 12, units = c("in"),dpi = 1200)

rm(list=ls())
dev.off()
