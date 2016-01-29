setwd("~/Desktop/project1")
##1.1 Setup the Bench
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")
##Read the data
cols<- c("ESR","ST","SEX","MSP","PWGTP")##choose the variable of employment,sex,marial status,state code and the weight
pusa <- fread("~/desktop/project1/csv_pus/ss13pusa.csv", select = cols)
pusb <- fread("~/desktop/project1/csv_pus/ss13pusb.csv", select = cols)
pus<- rbind(pusa, pusb)

Male <- pus %>%
  na.omit()%>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(SEX %in% c(1)) %>%
  group_by(ST) %>% ##group by state
  summarise(count=sum(PWGTP))%>%
  
  Male

Malejob<- pus %>%
  na.omit() %>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(ESR %in% c(1)) %>%
  filter(SEX %in% c(1)) %>% ## select male
  group_by(ST) %>% ##group by state
  summarise(count=sum(PWGTP))%>%
  
  Malejob


Female <- pus %>%
  na.omit()%>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(SEX %in% c(2)) %>%
  group_by(ST) %>% ##group by state
  summarise(count=sum(PWGTP))
Female

Femalejob<- pus %>%
  na.omit() %>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(ESR %in% c(1)) %>%
  filter(SEX %in% c(2)) %>% ## select male
  group_by(ST) %>% ##group by state
  summarise(count=sum(PWGTP))
Femalejob


Malejob1<- mutate(Male, value = Malejob$count/Male$count*100)
Malejob1
Femalejob1<- mutate(Femalejob, value = Femalejob$count/Female$count*100)
Femalejob1

# add gender to data
Malejob1$gender <- c("Male")
Femalejob1$gender <- c("Female")
# concat data
visual <- rbind(Malejob1,Femalejob1)
visual

#plot graph
AllPlot<- ggplot(visual, aes(x=gender, y=value, fill=factor(gender))) +
  geom_bar(stat="identity",position="dodge") + scale_fill_hue(l=40) +
  ylab("emplyoment ratio") + 
  xlab("gender") + ggtitle(paste("Employment ratio")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.background = element_rect(fill = 'white' ))
ggsave(paste("Plot_","_",".png", sep = ""), width = 20, height = 15)
AllPlot

