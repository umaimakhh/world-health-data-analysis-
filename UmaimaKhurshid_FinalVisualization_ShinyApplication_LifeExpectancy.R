library(ggplot2) # ggplot() for plotting
library(dplyr) # data reformatting
library(tidyr) # data reformatting
library(stringr) # string manipulation


setwd('E:/DPU/Data V project/Project Tables/combined/')
dataExpectancy = read.csv("underDevelopedAndDeveloped1.csv", header = TRUE,  sep = ",")
dataExpectancy$Life.expectancy.at.birth..total..years.<-as.numeric(dataExpectancy$Life.expectancy.at.birth..total..years.)
dataExpectancy$ï..Time<-as.numeric(dataExpectancy$ï..Time)
dataExpectancy<- na.omit(dataExpectancy)
na_sum <- function(x)
{
  if(all(is.na(x))) val <- sum(x,na.rm=F)
  if(!all(is.na(x))) val <- sum(x,na.rm=T)
  return(val)
}
m3 <- dataExpectancy %>%
  group_by(ï..Time,Country.Name) %>%
  summarise(count=na_sum(Life.expectancy.at.birth..total..years.)) %>%
  as.data.frame()

m4 <- m3 %>%

  # create a new variable from count
  mutate(countfactor=cut(count,breaks=c(-1,30,40,50,60,70,80,max(count,na.rm=T)),
                         labels=c("30","30-40","40-50","50-60","60-70","70-80",">80"))) %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))


library(RColorBrewer)
 ggplot(m4 ,aes(x=ï..Time,y=Country.Name,fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Life Expectany at Birth - Years"))+
  labs(x="",y="",title="Life expectancy of Developing and Under Developed Countries from 1960 - 2018")+  
  scale_fill_manual(values=rev(brewer.pal(7,"PuBuGn")),na.value="grey90")+
  scale_y_discrete(expand=c(0,0))+
  theme_grey(base_size=10)+
  theme(legend.position="top",legend.direction="horizontal",
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(size=7,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text=element_text(size=16,face="bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(vjust=0.2),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
    
        plot.title=element_text(hjust=0.5,size=14,face="bold"
        ))





