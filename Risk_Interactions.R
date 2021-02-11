rm(list=ls())
dev.off()
wd=getwd()
setwd(wd)
#install.packages("ggplot")
#install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
# plot a

norm <- function(x,mu,stdev) {
  (1/(stdev*sqrt(2*pi))*exp((-1/2)*((x-mu)/stdev)^2))
}

x=1:1000
y1=norm(x,500,25)
y2=norm(x,350,50)
y3=norm(x,650,50)
df1=rbind(data.frame(x,value=y1,group="y1"),data.frame(x,value=y2,group="y2"),data.frame(x,value=y3,group="y3"))
a<-ggplot(df1,aes(x,value,group=group))+
  geom_line(aes(color=group),size=2)+
  xlab("Water Levels")+
  ylab("Probability Density")+
  scale_colour_manual(name = '', 
                      values =c(y1='black',y3='red',y2='darkgreen'),
                      labels = c('Over Confident','Deep Uncertainty - High', 'Deep Uncertainty - Low'))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "top")
  
#plot b

logistic <- function(x,k,a){
  (1/(1+exp(-k*x)))^a
}
m=seq(from=-10,to=10,by=0.01)
n1=logistic(m,1,1)
n2=logistic(m,1,10)
df2=rbind(data.frame(m,value=n1,group="n1"),data.frame(m,value=n2,group="n2"))
b<-ggplot(df2,aes(m,value,group=group))+
  geom_line(aes(color=group),size=2)+
  xlab("Water Levels")+
  ylab("Damages")+
  scale_colour_manual(name = '', 
                      values =c(n2='blue',n1='red'),
                      labels = c('With Levee ???', 'Without Levee ???'))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "top")


#plot c

p=1:1000
q1=norm(p,200,50)
q2=norm(p,500,40)
q3=norm(p,800,50)
df3=rbind(data.frame(p,value=q1,group="q1"),data.frame(p,value=q2,group="q2"),data.frame(p,value=q3,group="q3"))

c<-ggplot(df3,aes(p,value,group=group))+
  geom_line(aes(color=group),size=2,linetype=c("dotted","solid","solid"))+
  xlab("Damages")+
  ylab("Probability Density")+
  scale_colour_manual(name = '', 
                      values =c(q1='blue',q2='black',q3='red'),
                      labels = c('Without Levee ??? - Low Confidence','With Levee ??? - Over Confident', 'With Levee ??? - High Confidence'))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "right")+
  coord_flip()

figure <- ggarrange(b,c,a,
                    labels = c("b)", "c)", "a)"),
                    font.label = list(size = 12,family="serif"),
                    vjust =1.5,
                    ncol = 2, nrow = 2)
figure
ggsave("Risk_Interactions.jpg",width = 11, height = 12, units = c("in"),dpi = 1200)
rm(list=ls())
dev.off()
