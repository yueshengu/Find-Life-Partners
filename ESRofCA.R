setwd("~/Desktop/project1")
##1.1 Setup the Bench
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")
##Read the data
cols<- c("ESR","ST","SEX","MSP","PWGTP","AGEP")##choose the variable of employment,sex,marial status,state code and the weight
pusa <- fread("~/desktop/project1/csv_pus/ss13pusa.csv", select = cols)
pusb <- fread("~/desktop/project1/csv_pus/ss13pusb.csv", select = cols)
pus<- rbind(pusa, pusb)

##Select the data
Male <- pus %>%
  na.omit()%>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(SEX %in% c(1)) %>%
  filter(AGEP<=29) %>%
  group_by(ST) %>% ##group by state
  summarise(count=sum(PWGTP))
Male

Malejob<- pus %>%
  na.omit() %>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(ESR %in% c(1)) %>%
  filter(SEX %in% c(1)) %>% ## select male
  filter(AGEP<=29) %>%
  group_by(ST) %>% ##group by state
  summarise(count=sum(PWGTP))

Malejob


Female <- pus %>%
  na.omit()%>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(SEX %in% c(2)) %>%
  filter(AGEP<=29) %>%
  group_by(ST) %>% ##group by state
  summarise(count=sum(PWGTP))
Female

Femalejob<- pus %>%
  na.omit() %>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(ESR %in% c(1)) %>%
  filter(SEX %in% c(2)) %>% ## select male
  filter(AGEP<=29) %>%
  group_by(ST) %>% ##group by state
  summarise(count=sum(PWGTP))
Femalejob

##define the state code
stateCode = "ST,State
6,California/CA"
statecode <- fread(stateCode)
statecode

Malejob1<- mutate(Male, value = Malejob$count/Male$count*100)
Malejob1
Malejob1[5]
#     ST   count    value
#(int)   (int)    (dbl)
# 1   6   3623480  48.75851
Femalejob1<- mutate(Femalejob, value = Femalejob$count/Female$count*100)
Femalejob1
Femalejob1[5]
#   ST   count    value
#(int)   (int)    (dbl)
#1  6  1546252  48.80578

# join state code in table
Malejob1 <- left_join(Malejob1 , statecode, by.x=c("ST"))
Femalejob1 <- left_join(Femalejob1 , statecode, by.x=c("ST"))
# add gender to data
Malejob1$gender <- c("Male")
Femalejob1$gender <- c("Female")
# concat data
visual <- rbind(Malejob1,Femalejob1)
#plot graph
AllPlot<- ggplot(visual, aes(x=State, y=value, fill=factor(gender))) +
  geom_bar(stat="identity",position="dodge") + scale_fill_hue(l=40) +
  ylab("emplyoment ratio") + 
  xlab("state") + ggtitle(paste("Employment ratio")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.background = element_rect(fill = 'white' ))
ggsave(paste("Plot_","_",".png", sep = ""), width = 20, height = 15)
AllPlot
##
##
#the employment rate of male in CA is 48.75851%
#the employment rate of female in CA is 48.80578%