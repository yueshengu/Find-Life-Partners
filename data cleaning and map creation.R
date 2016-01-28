library(rCharts)
library(htmltab)

# source mapping function
source('C:/Users/ygu/Desktop/columbia/cycle1-1/figs/modified choropleth.R')
actualPopulation<-
  htmltab("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population",1)

# class(pop1$FOD1P)
# head(pop1$FOD1P)
# nrow(pop1)+nrow(pop2) #3132795
# nrow(pop1[pop1$FOD1P==3702&!is.na(pop1$FOD1P),])+nrow(pop2[pop2$FOD1P==3702&!is.na(pop2$FOD1P),]) #499
# nrow(pop1[is.na(pop1$FOD1P),])+nrow(pop2[is.na(pop2$FOD1P),]) #2449467
# nrow(pop2)


############################################################################
############################## Data Cleaning ###############################

actualPopulation<-actualPopulation[1:52,3:4]
names(actualPopulation)<-c('StateName','ActualPop')
actualPopulation$State<-substring(actualPopulation$State,3)
actualPopulation$State[48]<-substring(actualPopulation$State[48],3)
actualPopulation$ActualPop<-as.numeric(gsub(',','',actualPopulation$ActualPop))

# combine pops
pop<-rbind(pop1,pop2)

# make MSP more readable
pop$MSP[is.na(pop$MSP)]<-'Under 15'
pop$MSP[pop$MSP==1]<-'Now married, spouse present'
pop$MSP[pop$MSP==2]<-'Now married, spouse absent'
pop$MSP[pop$MSP==3]<-'Widowed'
pop$MSP[pop$MSP==4]<-'Divorced'
pop$MSP[pop$MSP==5]<-'Separated'
pop$MSP[pop$MSP==6]<-'Never married'

# get state abbreviation
names(statename)[1]<-'ST'
pop2<-merge(pop,statename,by='ST',all.x=T)

# remove DC
# pop2<-pop2[pop2$abbr!='DC',]
# pop2

# create "single"
pop2$single<-'Single'
pop2$single[pop2$MSP%in%c('Under 15','Now married, spouse present','Now married, spouse absent')]<-
  'Married or too young'

#maritalState<-aggregate(pop2$MSP, by=list(pop2$MSP,pop2$abbr2,pop2$SEX), FUN=length)

maritalStateGeneral<-aggregate(pop2$PWGTP, by=list(pop2$single,pop2$abbr,pop2$SEX), FUN=sum)
names(maritalStateGeneral)<-c('Single','State','SEX','CountWithWeight')
state<-aggregate(pop2$PWGTP, by=list(pop2$abbr,pop2$name), FUN=sum)
names(state)<-c('State','StateName','TotalCountWithWeight')
maritalStateGeneral2<-merge(maritalStateGeneral,state,by='State',all.x=T)
maritalStateGeneral2$Perc<-round(maritalStateGeneral2$Count/maritalStateGeneral2$TotalCount*100,0)
maritalStateGeneral3<-merge(maritalStateGeneral2,actualPopulation,by='StateName',all.x=T)
maritalStateGeneral3$SEX[maritalStateGeneral3$SEX==1]<-'Male'
maritalStateGeneral3$SEX[maritalStateGeneral3$SEX==2]<-'Female'
maritalStateGeneral3$ExpectedCount2015<-round(maritalStateGeneral3$Perc*maritalStateGeneral3$ActualPop/100,0)

singles<-maritalStateGeneral3[maritalStateGeneral3$Single=='Single',]
singles<-singles[rev(order(singles$Perc)),]

write.csv(singles,'C:/Users/ygu/Desktop/columbia/cycle1-1/singles.csv',row.names=F)




require(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')
library(rMaps)
library(plyr);library(rCharts)

# easy maps
choro<-ichoropleth(Perc ~ State,legend=F,
                   data=maritalStateGeneral2[maritalStateGeneral2$SEX==1&maritalStateGeneral2$Single=='Single',])
choro$set(geographyConfig = list(
  popupTemplate = "#! function(geography, data){
  return '<div class=hoverinfo><strong>' + geography.properties.name + 
  ': ' + data.Perc + '% of state population are single male' + '</strong></div>';
  } !#" 
))
choro
choro$save('singleMales.html', cdn = TRUE)
choro$publish("Single Males plot")

ichoropleth(Perc ~ State, 
            data=maritalStateGeneral2[maritalStateGeneral2$SEX==2&maritalStateGeneral2$Single=='Single',],
            pal = 'PuRd')







g2<-ggplot(ag.mtc, aes(x = factor(gear), y = meanwt, fill=factor(vs))) + 
  geom_bar(stat = "identity", position=position_dodge()

ggplot(pop,aes(x=SEX),y=SEX,fill=factor(SEX),color=factor(SEX)) +  
  geom_bar(stat = "identity", position=position_dodge())
















# better plot but more work
male<-maritalStateGeneral2[maritalStateGeneral2$SEX==1&maritalStateGeneral2$Single=='Single',]
male$fillKey<-factor(paste0(male$Perc%/%2*2,'-',male$Perc%/%2*2+1,'%'))
male$State<-as.character(male$State)
fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'YlOrRd'), 'white'),
  c(paste0(7:11*2,'-',7:11*2+1,'%'), 'defaultFill')
)
y = toJSONArray2(male, json = F)
names(y) = lapply(y, '[[', 'State')

options(rcharts.cdn = TRUE)
map <- Datamaps$new()
map$set(
  dom = 'chart_1',
  scope = 'usa',
  fills = fills,
  data = y,
  legend = TRUE,
  labels = TRUE#,
#   geographyConfig = list(
#     popupTemplate = "#! function(geography, data){
#     return '<div class=hoverinfo><strong>' + geography.properties.name + 
#     ': ' + data.Perc + '</strong></div>';
#     } !#" 
#   )
)
map




























library(Quandl)
vcData = Quandl("FBI_UCR/USCRIME_TYPE_VIOLENTCRIMERATE")
library(reshape2)
datm <- melt(vcData, 'Year', 
             variable.name = 'State',
             value.name = 'Crime'
)
datm <- subset(na.omit(datm), 
               !(State %in% c("United States", "District of Columbia"))
)
datm2 <- transform(datm,
                   State = state.abb[match(as.character(State), state.name)],
                   fillKey = cut(Crime, quantile(Crime, seq(0, 1, 1/5)), labels = LETTERS[1:5]),
                   Year = as.numeric(substr(Year, 1, 4))
)
library(plyr);library(rCharts)
dat2 <- dlply(na.omit(datm2), "Year", function(x){
  y = toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'State')
  return(y)
})























popDegreNotNA<-pop[!is.na(pop$FOD1P),]
degreeSummary<-aggregate(popDegreNotNA[,1], by=list(popDegreNotNA$FOD1P), FUN=length)
degreeSummary<-degreeSummary[rev(order(degreeSummary$x)),]
degreeSummary$Perc<-degreeSummary$x/sum(degreeSummary$x)
degreeSummary[degreeSummary[,1]%in%c(3702,6212),]

popcow<-pop
popcow$COW[is.na(popcow$COW)]<-10
cowSummary<-aggregate(popcow$COW, by=list(popcow$COW), FUN=length)
cowSummary<-cowSummary[rev(order(cowSummary$x)),]
cowSummary$Perc<-cowSummary$x/sum(cowSummary$x)


citSummary<-aggregate(pop$CIT, by=list(pop$CIT), FUN=length)
citSummary<-citSummary[rev(order(citSummary$x)),]
citSummary$Perc<-citSummary$x/sum(citSummary$x)


maritalSummary<-aggregate(pop$MAR, by=list(pop$MAR), FUN=length)
maritalSummary<-maritalSummary[rev(order(maritalSummary$x)),]
maritalSummary$Perc<-maritalSummary$x/sum(maritalSummary$x)
maritalSummary

maritalSummary2<-aggregate(pop$MSP[!is.na(pop$MSP)], by=list(pop$MSP[!is.na(pop$MSP)]), FUN=length)
maritalSummary2<-maritalSummary2[rev(order(maritalSummary2$x)),]
maritalSummary2$Perc<-maritalSummary2$x/sum(maritalSummary2$x)
maritalSummary2



summary(pop$AGEP)
round(summary(factor(pop$AGEP))/nrow(pop),3)


under 15, married
geography
education
salary
hours worked
employment
citizenship
##ethnicity







ichoropleth(Crime ~ State, data = subset(violent_crime, Year == 2010))
ichoropleth(Crime ~ State, data = violent_crime, animate = "Year")
ichoropleth(Crime ~ State, data = violent_crime, animate = "Year", play = TRUE)




























