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

#########
Total<- pus %>%
  na.omit()%>%
  filter(MSP %in% c(3,4,5,6)) %>%
  group_by(SEX) %>% ##group by state
  summarise(count=sum(PWGTP))
Total

Employment<- pus %>% 
na.omit() %>%
  filter(MSP %in% c(3,4,5,6)) %>%
  filter(ESR %in% c(1)) %>%
  group_by(SEX) %>% ##group by state
  summarise(count=sum(PWGTP))
Employment

##define the gender code
genderCode = "SEX,gender
1,Male
2,Female"
gendercode <- fread(genderCode)
gendercode

#Visualize   them here
job<- mutate(Employment, value = Employment$count/Total$count*100)
job<- left_join(job, gendercode, by.x=c("SEX"))
job
#     SEX gender  count    value
#(int)  (chr)    (int)    (dbl)
#1     1   Male 32559654 53.79506
#2     2 Female 34225477 50.11657
> 
gender<- factor(job$gender, levels = unique(job$gender))


#plot graph
AllPlot<- ggplot(job, aes(x=gender, y=value, fill=factor(gender))) +
  geom_bar(stat="identity",position="dodge") + scale_fill_hue(l=40) +
  ylab("emplyoment ratio") + 
  xlab("gender") + ggtitle(paste("Employment ratio")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.background = element_rect(fill = 'white' ))
ggsave(paste("Plot_","_",".png", sep = ""), width = 20, height = 15)
AllPlot
###
# the employment ratio of male in all states is 53.79506%
# the employment ratio of female in all states is 50.11657%

