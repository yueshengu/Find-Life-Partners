#load the library we need
library(data.table)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
#set up working directory
setwd("/Users/ruixiongshi/Documents")

#select the columns we need
colstokeep<-c("PWGTP", "ST", "CIT", "DOUT", "LANX", "SEX", "INDP", "MSP")

#read data
housea<-fread("ss13pusa.csv", select=colstokeep)
houseb<-fread("ss13pusb.csv", select=colstokeep)
popdata<-rbind(housea,houseb)
rm(housea,houseb)

#excluding married people, and missing values
sindata<-tbl_df(popdata)
sindata<-sindata %>%
  na.omit() %>%
  filter(MSP %in% c(3,4,5,6)) %>% #code 1&2 are married
  group_by(SEX)
summary(sindata)

#transfer the code 1&2 into male&female
sindata$SEX[sindata$SEX==1]<-"Male"
sindata$SEX[sindata$SEX==2]<-"Female"

#Compute the weighted count for single people and convert it to percentage
weisingledata<-sindata %>% count(SEX, wt=PWGTP)
weisingledata$n <- weisingledata$n/sum(sindata$PWGTP)


#Draw the graph of single people by gender
genderplot<-ggplot(weisingledata,aes(x=SEX, y=n, fill=as.factor(SEX)))+geom_bar(stat="identity")+ggtitle("Comparing single people in US by gender")
genderplot<-genderplot+scale_fill_discrete(name = "Gender")+xlab("Gender")+ylab("Percentage of people")
genderplot
ggsave("genderplot.png")

#Compute the percentage of weighted single people over total population
sum(sindata$PWGTP)/sum(popdata$PWGTP) #gives 0.2916053 about 30%
