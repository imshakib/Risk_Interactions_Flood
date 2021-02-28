##==============================================================================
##
## Script creates conceptual graphs to show interactions among flood risk hazard, 
## vulnerability and risk
##
## Authors: Iman Hosseini-Shakib (ishakib@gmail.com)
##          Klaus Keller (kzk10@psu.edu)
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
wd=getwd()
setwd(wd)

# Packages Used
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("bayestestR")
#install.packages("VGAM")

library(ggplot2)
library(ggpubr)
library(bayestestR)
library(VGAM)

# A logistic function to mimic the S-shaped depth-damage curve with water levels (WL) and coefficients a and k
logistic <- function(WL,k,a){
  (1/(1+exp(-k*WL)))^a
} 

# Parameters

  # Water level distribution characteristics
WL=1:1000   #hypothetical water levels
meanWL=525 #hypothetical mean water level for the stationary pdf
sdWL=65 #hypothetical standard deviation of the stationary pdf
meanWL_nonS=550 #hypothetical mean water level for the non-stationary pdf
sdWL_nonS=150 #hypothetical standard deviation of the non-stationary pdf
shapeWL_nonS=5 #hypothetical shape factor of the non-stationary pdf

  # Hazard
WLbest=WL[meanWL] #hypothetical best guess stationary water level
WL_PDF1=dnorm(WLbest,meanWL,sdWL) #hypothetical best guess stationary water level probability
WL_PDF2=dnorm(WL,meanWL,sdWL) #hypothetical distribution of water levels - stationary with uncertainty
WL_PDF3=dskewnorm(WL,location=meanWL_nonS,scale=sdWL_nonS,shape=shapeWL_nonS) #hypothetical distribution of water levels - non-stationary with uncertainty

# Vulnerability
DMG2=logistic((0.015*WL)-11,0.8,0.6) #hypothetical damages - without levee effect
LEV=1.3 #hypothetical levee effect coefficient
DMG1=DMG2*LEV #hypothetical damages - with levee effect 

  # Risk
RSK1=WL_PDF1*DMG2[WLbest] # hypothetical risk with stationary best guess hazard and depth-damage function without levee effect (WL_PDF1 and DMG2)
RSK2=WL_PDF3*DMG1 # hypothetical risk pdf with non-stationary with uncertainty pdf and depth-damage function with levee effect (WL_PDF3 and DMG1)
RSK3=WL_PDF3*DMG2 # hypothetical risk pdf with non-stationary with uncertainty pdf and depth-damage function without levee effect (WL_PDF3 and DMG2)
RSK4=WL_PDF2*DMG2 # hypothetical risk pdf with stationary with uncertainty pdf and depth-damage function without levee effect (WL_PDF2 and DMG2)

    #Normalizing risk pdfs
area_RSK2=area_under_curve(DMG1,RSK2) # area under the RSK2 curve
area_RSK3=area_under_curve(DMG2,RSK3) # area under the RSK3 curve 
area_RSK4=area_under_curve(DMG2,RSK4) # area under the RSK4 curve

c_RSK2=max(area_RSK2,area_RSK3,area_RSK4)/area_RSK2 # correction factor for RSK2 pdf area
c_RSK3=max(area_RSK2,area_RSK3,area_RSK4)/area_RSK3 # correction factor for RSK3 pdf area
c_RSK4=max(area_RSK2,area_RSK3,area_RSK4)/area_RSK4 # correction factor for RSK4 pdf area

RSK2=RSK2*c_RSK2 #area correction for RSK2
RSK3=RSK3*c_RSK3 #area correction for RSK3
RSK4=RSK4*c_RSK4 #area correction for RSK4

    #Finding risk pdf means (where cdf=0.5)
area2=area3=area4=data.frame()
area2[1,1]=area3[1,1]=area4[1,1]=0
for(i in 2:length(WL)){
  area2[i,1]=(0.5*(RSK2[i]+RSK2[i-1]))*(DMG1[i]-DMG1[i-1])
  area3[i,1]=(0.5*(RSK3[i]+RSK3[i-1]))*(DMG2[i]-DMG2[i-1])
  area4[i,1]=(0.5*(RSK4[i]+RSK4[i-1]))*(DMG2[i]-DMG2[i-1])
}

mean_RSK2=data.frame(DMG=DMG1,RSK=RSK2,CUMU=abs(cumsum(area2$V1)-0.5*sum(area2$V1)))
mean_RSK2=mean_RSK2[mean_RSK2$CUMU==min(mean_RSK2$CUMU),]
mean_RSK3=data.frame(DMG=DMG2,RSK=RSK3,CUMU=abs(cumsum(area3$V1)-0.5*sum(area3$V1)))
mean_RSK3=mean_RSK3[mean_RSK3$CUMU==min(mean_RSK3$CUMU),]
mean_RSK4=data.frame(DMG=DMG2,RSK=RSK4,CUMU=abs(cumsum(area4$V1)-0.5*sum(area4$V1)))
mean_RSK4=mean_RSK4[mean_RSK4$CUMU==min(mean_RSK4$CUMU),]

# Plot a - Hazard
df1=rbind(data.frame(WL,value=WL_PDF2,group="WL_PDF2"),
          data.frame(WL,value=WL_PDF3,group="WL_PDF3"))

a<-ggplot(df1,aes(WL,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  geom_segment(x = WLbest,y=0,xend=WLbest,yend=max(df1$value),size=1,linetype="solid")+ #hypothetical best guess stationary water levels
  xlab("Water Levels")+
  ylab("Probability Density")+
  scale_colour_manual(name = '', 
                      values =c(WL_PDF1='black',WL_PDF3='darkblue',WL_PDF2='cornflowerblue'),
                      labels = c('Stationary best guess','non-stationary with uncertainty', 'Stationary with uncertainty'))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1*max(df1$WL))) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.25*max(df1$value))) + 
  geom_text(aes(x=580,y=1*max(value)),
            label="Neglecting\nclimate change\nand uncertainty",
            color="black",size=6,lineheight = .75,hjust="left")+
  geom_text(aes(x=725,y=0.004),
            label="Considering\nclimate change\nand uncertainty",
            color="darkblue",size=6,lineheight = .75,hjust="left")+
  geom_text(aes(x=450,y=0.0055),
            label="Neglecting\nclimate change\nconsidering\nuncertainty", 
            color="cornflowerblue",size=6,lineheight = .75,hjust="right")+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        legend.text=element_text(size=18),
        legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = arrow(angle = 15, length = unit(0.2, "inches"),type = "closed")))

# Plot b - Vulnerability
df2=rbind(data.frame(WL,value=DMG1,group="DMG1"),data.frame(WL,value=DMG2,group="DMG2"))

b<-ggplot(df2,aes(WL,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  xlab("Water Levels")+
  ylab("Damages")+
  scale_colour_manual(name = '', 
                      values =c(DMG2='darkkhaki',DMG1='darkgreen'),
                      labels = c('With Levee Effect', 'Without Levee Effect'))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1*max(df2$WL))) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1*max(df2$value))) + 
  geom_text(aes(x=525,y=0.6*max(value)),
            label="Considering\nlevee effect", color="darkgreen",
            size=6,lineheight = .75)+
  geom_text(aes(x=775,y=0.25*max(value)),
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
df3=rbind(data.frame(p=DMG1,value=RSK2,group="RSK2"),
          data.frame(p=DMG2,value=RSK3,group="RSK3"),
          data.frame(p=DMG2,value=RSK4,group="RSK4"))

c<-ggplot(df3,aes(p,value,group=group))+
  geom_line(aes(color=group),size=1.5)+
  geom_segment(x=DMG2[WLbest],y=0,xend=DMG2[WLbest],yend=1.9*max(RSK4),size=1,linetype="solid")+ #hypothetical best guess stationary water levels
  geom_point(aes(x=DMG2[WLbest],y=1.95*max(RSK4)),size=6,color="black")+
  geom_point(aes(x=mean_RSK2[1,1],y=1.95*max(RSK4)),size=6,color="darkred")+
  geom_point(aes(x=mean_RSK3[1,1],y=1.95*max(RSK4)),size=6,color="orangered1")+
  geom_point(aes(x=mean_RSK4[1,1],y=1.95*max(RSK4)),size=6,color="orange")+
  xlab("Damages")+
  ylab("Probability Density")+
  scale_colour_manual(name = '', 
                      values =c(RSK1='black',RSK2='darkred',RSK3='orangered1',RSK4='orange'),
                      labels = c('Stationary best guess neglecting levee effect','Non-stationary with uncertainty considering levee effect',
                                 'Non-stationary with uncertainty neglecting levee effect', "Stationary with uncertainty neglecting levee effect"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1*max(df3$p))) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2*max(df3$value))) + 
  
  geom_segment(x=0.01*max(df3$p),y=1.89*max(df3$value),xend = 0.13*max(df3$p),yend = 1.95*max(df3$value),
               size=0.5,arrow = arrow(angle = 15, length = unit(0.1, "inches")))+  
  geom_label(aes(x=0.08*max(p),y=1.68*max(value)),label="Expected\nvalues",
             size=6,hjust="center",lineheight = .75,fill="lightgray")+
  
  geom_text(aes(x=0.22*max(p),y=1.65*max(value)),
            label="Neglecting all",
            color="black",size=6,hjust="right",lineheight = .75)+
  geom_text(aes(x=0.87*max(p),y=0.25*max(value)),
            label="Considering all",
            hjust="left",color="darkred",size=6,lineheight = .75)+
  geom_text(aes(x=0.63*max(p),y=0.45*max(value)),
            label="Neglecting levee effect,\nconsidering climate change\nand uncertainty", 
            hjust="left",color="orangered1",size=6,lineheight = .75)+
  geom_text(aes(x=0.4*max(p),y=0.58*max(value)),
            label="Neglecting climate change\nand levee effect,\nconsidering uncertainty", 
            hjust="left",color="orange",size=6,lineheight = .75)+
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
ggsave("Risk_Interactions_20210227.jpg",width = 11, height = 8.5, units = c("in"),dpi = 600)
ggsave("Risk_Interactions_20210227.pdf",width = 11, height = 8.5, units = c("in"),dpi = 600)

# Time to clean up!
rm(list=ls())
