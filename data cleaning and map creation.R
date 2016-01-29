library(rCharts)
library(htmltab)

# source mapping function
source('C:/Users/ygu/Desktop/columbia/cycle1-1/figs/modified choropleth.R')


actualPopulation<-
  htmltab("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population",1)

pop1<-read.csv('C:/Users/ygu/Desktop/columbia/csv_pus/ss13pusa.csv')
pop2<-read.csv('C:/Users/ygu/Desktop/columbia/csv_pus/ss13pusb.csv')
statename<-read.csv('C:/Users/ygu/Desktop/columbia/cycle1-1/data/statename.csv')

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
actualPopulation$StateName<-substring(actualPopulation$StateName,3)
actualPopulation$StateName[48]<-substring(actualPopulation$StateName[48],3)
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

# removing DC for mapping purpose
pop3<-pop2[pop2$abbr!='DC',]
save(pop3,file='C:/Users/ygu/Desktop/columbia/findingLifePartner/www/pop3.RData')



pop4<-pop3[,c('PWGTP','AGEP','CIT','COW','SCHL','SEX','WAGP','WKHP','MSP','single','abbr','RAC1P',
              'FSCHP','name')]

pop4$CIT[pop4$CIT==1]<-'Born in the U.S'
pop4$CIT[pop4$CIT==2]<-'Born in Puerto Rico, Guam, the U.S. Virgin Islands, or the Northern Marianas'
pop4$CIT[pop4$CIT==3]<-'Born abroad of American parent(s)'
pop4$CIT[pop4$CIT==4]<-'U.S. citizen by naturalization'
pop4$CIT[pop4$CIT==5]<-'Not a citizen of the U.S.'
pop4$CIT<-factor(pop4$CIT)

pop4$COW[is.na(pop4$COW)]<-'Less than 16 years old'
pop4$COW[pop4$COW==1]<-'Employee of a private for-profit company'
pop4$COW[pop4$COW==2]<-'Employee of a private not-for-profit'
pop4$COW[pop4$COW==3]<-'Local government employee'
pop4$COW[pop4$COW==4]<-'State government employee'
pop4$COW[pop4$COW==5]<-'Federal government employee'
pop4$COW[pop4$COW==6]<-'Self-employed in own not incorporated'
pop4$COW[pop4$COW==7]<-'Self-employed in own incorporated'
pop4$COW[pop4$COW==8]<-'Working without pay in family business or farm'
pop4$COW[pop4$COW==9]<-'Unemployed and last worked 5 years ago or earlier'
pop4$COW<-factor(pop4$COW)

pop4$SCHL[is.na(pop4$SCHL)]<-'Less than 3 years old'
pop4$SCHL[pop4$SCHL==1]<-'No schooling completed'
pop4$SCHL[pop4$SCHL%in%c(2:3)]<-'Nursery school, preschool, Kindergarten'
pop4$SCHL[pop4$SCHL%in%c(4:15)]<-'Grade 1-12 and no diploma'
pop4$SCHL[pop4$SCHL==16]<-'Regular high school diploma'
pop4$SCHL[pop4$SCHL==17]<-'GED or alternative credential'
pop4$SCHL[pop4$SCHL%in%c(18:19)]<-'Some college, no degree'
pop4$SCHL[pop4$SCHL==20]<-"Associate's degree"
pop4$SCHL[pop4$SCHL==21]<-"Bachelor's degree"
pop4$SCHL[pop4$SCHL==22]<-"Master's degree"
pop4$SCHL[pop4$SCHL==23]<-"Professional degree beyond a bachelor's degree"
pop4$SCHL[pop4$SCHL==24]<-'Doctorate degree'
pop4$SCHL<-factor(pop4$SCHL)

pop4$SEX[pop4$SEX==1]<-'Male'
pop4$SEX[pop4$SEX==2]<-'Female'
pop4$SEX<-factor(pop4$SEX)

pop4$RAC1P[pop4$RAC1P==1]<-'White'
pop4$RAC1P[pop4$RAC1P==2]<-'Black or African American'
pop4$RAC1P[pop4$RAC1P%in%c(3:5)]<-'American Indian or Alaska Native'
pop4$RAC1P[pop4$RAC1P==6]<-'Asian'
pop4$RAC1P[pop4$RAC1P==7]<-'Hawaiian and Other Pacific Islander'
pop4$RAC1P[pop4$RAC1P==8]<-'Other Race'
pop4$RAC1P[pop4$RAC1P==9]<-'2 or More Races'
pop4$RAC1P<-factor(pop4$RAC1P)

pop4$MSP<-factor(pop4$MSP)
pop4$single<-factor(pop4$single)

save(pop4,file='C:/Users/ygu/Desktop/columbia/findingLifePartner/www/pop4.RData')



state<-aggregate(pop3$PWGTP, by=list(pop3$abbr,pop3$name), FUN=sum)
names(state)<-c('State','StateName','TotalCountWithWeight')

# Plotting sugar daddy
sugarDaddy<-pop3[pop3$single=='Single'&pop3$SEX==1&!is.na(pop3$WAGP)&pop3$WAGP>=10000,]
sugarDaddy2<-aggregate(sugarDaddy$PWGTP,by=list(sugarDaddy$abbr), FUN=sum)
names(sugarDaddy2)<-c('State','CountWithWeight')
sugarDaddy3<-merge(sugarDaddy2,state,by='State',all.x=T)
sugarDaddy3$Perc<-round(sugarDaddy3$Count/sugarDaddy3$TotalCount*100,1)
sugarDaddy4<-merge(sugarDaddy3,actualPopulation,by='StateName',all.x=T)
sugarDaddy4$ExpectedCount2015<-round(sugarDaddy4$Perc*sugarDaddy4$ActualPop/100,0)

require(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')
library(rMaps)
library(plyr);library(rCharts)

# easy maps
choro<-ichoropleth(Perc ~ State,legend=T,pal='YlOrRd',data=sugarDaddy4)
choro$set(geographyConfig = list(
  popupTemplate = "#! function(geography, data){
  return '<div class=hoverinfo><strong>' + geography.properties.name + 
  ': ' + data.Perc + '% of state population are single male' + '</strong></div>';
  } !#" 
))
choro
ichoropleth(ExpectedCount2015 ~ State,legend=T,pal='YlOrRd',data=sugarDaddy4)
# choro$save('singleMales.html', cdn = TRUE)
# choro$publish("Single Males plot")


# plotting perfect girl
pgirl<-pop3[pop3$single=='Single'&pop3$SEX==2&pop3$AGEP<30&!is.na(pop3$SCHL)&pop3$SCHL>=21&!is.na(pop3$ESR)&
              pop3$ESR%in%c(1,2,4,5),]
pgirl2<-aggregate(pgirl$PWGTP,by=list(pgirl$abbr), FUN=sum)
names(pgirl2)<-c('State','CountWithWeight')
pgirl3<-merge(pgirl2,state,by='State',all.x=T)
pgirl3$Perc<-round(pgirl3$Count/pgirl3$TotalCount*100,1)
pgirl4<-merge(pgirl3,actualPopulation,by='StateName',all.x=T)
pgirl4$ExpectedCount2015<-round(pgirl4$Perc*pgirl4$ActualPop/100,0)

ichoropleth(Perc ~ State,data=pgirl4,pal = 'PuRd')
ichoropleth(ExpectedCount2015 ~ State,data=pgirl4,pal = 'PuRd')



knit('C:/Users/ygu/Desktop/columbia/cycle1-1/Finding Life Partner.Rmd')


library(leaflet)
library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

a








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




























