library(survey)
library(dplyr)
library(data.table)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)

colsforedu<-c("SCHL","ST","SEX","MSP","AGEP","PWGTP")
degree<-read.csv("F:/project1/educationlevel.csv")
ST.anno<-fread("F:/project1/statesname.csv")
pusa_edu<-fread("F:/project1/ss13pusa.csv",select=colsforedu)
pusb_edu<-fread("F:/project1/ss13pusb.csv",select=colsforedu)
pus_edu<-rbind(pusa_edu,pusb_edu)

#*************************percentage of single men/women in different educational level
per_edu<-right_join(pus_edu,degree,by.x=c("SCHL"))
per_eduman=
  per_eduman%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2)%>%
  filter(SEX==1)%>%
  group_by(no)%>%
  summarise(count=sum(PWGTP))
per_eduman$count<-per_eduman$count/sum(per_eduman$count)

per_eduwm=
  per_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2)%>%
  filter(SEX==2)%>%
  group_by(no)%>%
  summarise(count=sum(PWGTP))
per_eduwm$count<-per_eduwm$count/sum(per_eduwm$count)

layout(matrix(c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)), 4, 4, byrow=T)
legend_text<-
  c("no degree","Associate's degree","Bachelor's degree","Master's degree","Professional degree","Doctorate degree")
barplot(per_eduman$count,legend.text=legend_text,ylim = c(0, 0.8),
        col=terrain.colors(3),main="Percentage of Single Men's Education",
        xlab="level",ylab="percentage")
barplot(per_eduwm$count,legend.text=legend_text, ylim = c(0, 0.8),
        col=heat.colors(3),main="Percentage of Single Women's Education",
        xlab="level",ylab="percentage")

#single men with different educational levels
pure_edum_no=
  pus_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2)%>%
  filter(SEX==1)%>%
  group_by(ST)%>%
  summarise(count=sum(PWGTP))
            
#single men with Associate's degrees or above 
pure_edumhigh_no=
  pus_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2)%>%
  filter(SEX==1 & SCHL>=20)%>%
  group_by(ST)%>%
  summarise(count=sum(PWGTP))
pure_edumhigh_no[is.na(pure_edumhigh_no)] <- 0
pure_edumhigh<-mutate(pure_edumhigh_no,value=pure_edumhigh_no$count/pure_edum_no$count*100)
pure_edumhigh<-left_join(pure_edumhigh,ST.anno, by.x=c("ST"))

#single women with different educational level
pure_eduwm_no=
  pus_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2)%>%
  filter(SEX==2)%>%
  group_by(ST)%>%
  summarise(count=sum(PWGTP))

#single women with Associate's degrees or above 
pure_eduwmhigh_no=
  pus_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2)%>%
  filter(SEX==2 & SCHL>=20)%>%
  group_by(ST)%>%
  summarise(count=sum(PWGTP))
pure_eduwmhigh_no[is.na(pure_eduwmhigh_no)] <- 0
pure_eduwmhigh<-mutate(pure_eduwmhigh_no,value=pure_eduwmhigh_no$count/pure_eduwm_no$count*100)
pure_eduwmhigh<-left_join(pure_eduwmhigh,ST.anno, by.x=c("ST"))

#plot the percentage of men/women with Bachelor Degree or above
state_choropleth(pure_edumhigh,title = "Percentage of Single Men with Associate's Degree or Above",num_colors=9)
state_choropleth(pure_eduwmhigh,title = "Percentage of Single Women with Associate's Degree or Above",num_colors=9)
View(pure_edumhigh)
View(pure_eduwmhigh)

write.csv(pure_edumhigh,file="F:/project1/pure_edumhigh.csv")
write.csv(pure_eduwmhigh,file="F:/project1/pure_eduwmhigh.csv")

#*******************number of high educational-level men in cities
ny_edum=
  pus_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2 & SCHL>=20)%>%
  filter(SEX==1 & ST==36)%>%
  group_by(SCHL)%>%
  summarise(count=sum(PWGTP))%>%
  mutate(.,city=rep("ny",5))

ca_edum=
  pus_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2 & SCHL>=20)%>%
  filter(SEX==1 & ST==6)%>%
  group_by(SCHL)%>%
  summarise(count=sum(PWGTP))%>%
  mutate(.,city=rep("ca",5))
comparem_nyca<-rbind(ny_edum,ca_edum)
comparem_nyca<-right_join(comparem_nyca,degree,by.x=c("SCHL"))


ggplot(comparem_nyca, aes(x = city, y = count, group = degree)) + 
  geom_bar(stat = "identity", aes(colour = degree, fill = degree), alpha = 0.3) + 
  labs(x = "city", y = "count", title = "number of high educational-level men in cities")

#*************************number of high educational-level women in cities
ny_eduwm=
  pus_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2 & SCHL>=20)%>%
  filter(SEX==2 & ST==36)%>%
  group_by(SCHL)%>%
  summarise(count=sum(PWGTP))%>%
  mutate(.,city=rep("ny",5))

ca_eduwm=
  pus_edu%>%
  na.omit() %>%
  filter(AGEP>=15 & MSP>2 & SCHL>=20)%>%
  filter(SEX==2 & ST==6)%>%
  group_by(SCHL)%>%
  summarise(count=sum(PWGTP))%>%
  mutate(.,city=rep("ca",5))
comparewm_nyca<-rbind(ny_eduwm,ca_eduwm)


degree$SCHL<-as.numeric(as.character(degree$SCHL))
comparewm_nyca<-right_join(comparewm_nyca,degree,by.x=c("SCHL"))

ggplot(comparewm_nyca, aes(x = city, y = count, group = degree)) + 
  geom_bar(stat = "identity", aes(colour = degree, fill = degree), alpha = 0.3) + 
  labs(x = "city", y = "count", title = "number of high educational-level women in cities")
