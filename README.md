# 1 Introduction
====================

<p>In this script we are interested to see some general features of the singles (including those who are separated, widowed and divorced) </p>

<p>Then we will find out that as a graduated master student, assuming I am free to choose the place to work, where would be the best states to meet my life partner? 

Our ultimate goal is end up with a search engine that can show you the best state based on your preference of life partner, by assuming he/she would make rational rather than emotional decision.

To answer questions in the first part, we focus on their gender ratio, their age distribution, employment status, income and education level. </p>

### 1.1 Libraries we need
==========================

``` r
library(rCharts)
library(htmltab)
library(rMaps)
library(rCharts)
library(ggplot2)
library(dplyr)
```
### 1.2 Load the data 
==========================

<p>To save time, read the data select variables of interest and store it as RData for future use</p>

``` r 
colstokeep<-c("PWGTP", "ST", "SCHL", "AGEP", "SEX", "ESR", "MSP", "WAGP",'CIT','COW','WKHP','RAC1P','FSCHP') 
pop1<-fread('ss13pusa.csv', select = colstokeep)
pop2<-fread('ss13pusb.csv',select = colstokeep)
pop4<-rbind(pop1,pop2)
rm(pop1, pop2)
  save(pop4, file="pop4.RData")
}else{
  load("pop4.RData")
} 
```
### 1.3 Data manipulation
==========================

<p>Removing all the data with NA value in some columns and removing people who are married or under 15. Then group them accordingly</p>

``` r
#excluding married people, and missing values
sindata<-tbl_df(pop4)
sindata<-sindata %>%
  na.omit() %>%
  filter(MSP %in% c('Widowed','Divorced','Separated','Never married')) %>% #code 1&2 are married
  group_by(SEX)
```

# 2 General information of single population
====================================================

### 2.1 How many are they?
==========================

<p>First of all we want to know the percentage of the single people all over the state. </p>

``` r
#Compute the percentage of weighted single people over weighted population
sum(sindata$PWGTP)/sum(pop4$PWGTP)
```
    ## [1] 0.2515322
    
<p>Great! about 25% of population are single then we would like are single men more than women? </p>

### 2.2 The women/men ratio
==========================

``` r
#Draw the pie chart of single people by gender
genderdata<-aggregate(sindata$PWGTP,list(sindata$SEX),FUN=sum)
names(genderdata)<-c('SEX','Count') # returns female with 40110995 and male with 39512549
```

![](doc/image/singlePie.png) 

</p>We can see that women slightly outnumber men. Next, we would like to see their age distribution. </p>

### 2.3 How old are they?
==========================

``` r
genderageplot<-ggplot(sindata,aes(x=SEX, y=AGEP, fill=as.factor(SEX)))+geom_boxplot()+ggtitle("Comparing age of single people in US by gender") + scale_fill_discrete(name = "Gender")+xlab("Gender")+ylab("Age") + scale_y_continuous(limits=c(0,100))+theme_bw()
```

![](doc/image/singleAge.png)

<p>we can see women performs generally better then men in finding a job. The unemployment rate drops as the age increase. If you are looking for a partner under 30 years old and you do not want him/her without a job, then you are in the dangerous zone</p>

<p>It is clear that median age of single women are 5 years older than single men. If you are a lady who want to find a boyfriend under 30, then half of the single gentlemen is waiting for you. If you are a gentleman who look for ladies under 25 then 25% of total single ladies is waiting for you. </p>

<b><i>The next question is, are they good husband/wife for you? </i>.</b>

### 2.4 How about their employment status, income and education level?
==============================================================================

#### 2.4.1 Employment status according to age</b>
====================================================

<p>Though some people in the survey are marked as "with a job but not at work", in this part we choose only the person marked as unemployed</p>

``` r
unempl<-empl[empl$ESR==3,] #choose the people only marked as unemployed
unempl2<-aggregate(unempl$PWGTP,by=list(unempl$AGEP,unempl$SEX),FUN=sum)
names(unempl2)<-c('Age','Sex','UnemplCount')
unempl2$id<-paste0(unempl2$Age,unempl2$Sex)
```

![](doc/image/unemplAge.png)

<p>we can see women perform generally better then men in finding a job. The unemployment rate drops as the age increase. If you are looking for a partner under 30 years old and you do not want him/her without a job, then you are in the dangerous zone</p>

#### 2.4.2 Income level
==========================

<p>We break their income into 6 levels and calculate the weighted count</p>

``` r
#break the WAGP (lower=0, upper=100000, by=20000)
popudata$WAGP2[popudata$WAGP %in% c(0:20000)] <- "0-20k"
popudata$WAGP2[popudata$WAGP %in% c(20000:40000)] <- "20-40k"
popudata$WAGP2[popudata$WAGP %in% c(40000:60000)] <- "40-60k"
popudata$WAGP2[popudata$WAGP %in% c(60000:80000)] <- "60-80k"
popudata$WAGP2[popudata$WAGP %in% c(80000:100000)] <- "80-100k"
popudata$WAGP2[popudata$WAGP %in% c(100000:1000000)] <- "over 100k"

#sum the weights
popudata<-aggregate(popudata$PWGTP,by=list(popudata$WAGP2,popudata$SEX),FUN=sum)
names(popudata)<-c('WAGP','SEX','PWGTP')
```

![](doc/image/salary.png)

<<p>From the above plot, the low paid group (under 20k annual), for both female and male, dominate all other groups. </p>

#### 2.4.3 Education level
============================

<p>Change the SCHL codes into different college degrees </p>

``` r
per_edu$SCHL[per_edu$SCHL < 20] <- "No degree"
per_edu$SCHL[per_edu$SCHL == 20] <- "Associated degree"
per_edu$SCHL[per_edu$SCHL == 21] <- "Bachelor's degree"
per_edu$SCHL[per_edu$SCHL == 22] <- "Master's degree"
per_edu$SCHL[per_edu$SCHL == 23] <- "Professional degree beyond a bachelor's degree"
per_edu$SCHL[per_edu$SCHL == 24] <- "Doctorate degree"
```

![](doc/image/education.png)

<p>More than half of single people do not have a college degree. </p>

<p>According to the "should I do a PhD?" post, we can see that as a master student, our annual salary after graduation is about 50k annual. If you are looking for someone who has similar qualification and income like you by randomly picking from single population, then you are wasting your time. </p>

# 3 In which states can I find him/her?
====================================================

<p>As we have seen, finding a good partner is an intense competition actually, if you want to stand out, you must have some tricks. </p>

<b>Here is the trick: we are going to locate one or two states which has the largest number of good partners and highest proportion of them among total state population. </b>

<p>Now imagine I am a young lady from statistics department, assuming I am free to find a job in every state and my boyfriend criterions are:</p>

<i>"I want him to have an annual income over 100K, I do not care his age, education or whether he has married before. Basically I just want a sugar daddy. Which state should I find him?" </i>

### 3.1 Some library changes for this part
====================================================

``` r
detach(package:dplyr,unload=T)
library(plyr)# these two packages are clashed in some functions
```

### 3.2 Targeting the best state
=======================================

``` r 
state<-aggregate(pop3$PWGTP, by=list(pop3$abbr,pop3$name), FUN=sum)
names(state)<-c('State','StateName','TotalCountWithWeight')

# Plotting sugar daddy
sugarDaddy<-pop3[pop3$single=='Single'&pop3$SEX==1&!is.na(pop3$WAGP)&pop3$WAGP>=100000,]
sugarDaddy2<-aggregate(sugarDaddy$PWGTP,by=list(sugarDaddy$abbr), FUN=sum)
names(sugarDaddy2)<-c('State','CountWithWeight')
sugarDaddy3<-merge(sugarDaddy2,state,by='State',all.x=T)
sugarDaddy3$Perc<-round(sugarDaddy3$Count/sugarDaddy3$TotalCount*100,1)
sugarDaddy4<-merge(sugarDaddy3,actualPopulation,by='StateName',all.x=T)
sugarDaddy4$ExpectedCount2015<-round(sugarDaddy4$Perc*sugarDaddy4$ActualPop/100,0)

ichoropleth(Perc ~ State,legend=T,pal='YlOrRd',data=sugarDaddy4)
```

<b>Expected percentage of single males earning more than 100K annually</b>

<p align="left"><img src="doc/image/sugarDaddyPerc.png" ></p>

``` r 
ichoropleth(ExpectedCount2015 ~ State,legend=T,pal='YlOrRd',data=sugarDaddy4)
```

<b>Expected total number of single males earning more than 100K annually</b>

<p align="left"><img src="doc/image/sugarDaddyNum.png" ></p>

<p>It seems like New York, California and Massachuset are the best states for hunting them. Great! </p>


# 4 Which industries are the best?
=======================================

<p>In order to find my sugar daddy, I decide to live in either New York state or California to meet my sugar daddy then I need to know where can I meet him. The best idea will be we work together. So the next step I will figure out where they work. Luckily, data scientist can work in various field. </p>

``` r 
# look at industry of sugar daddys in CA and NY
sugardaddyNYCA<-pop4[pop4$single=='Single'&pop4$SEX=='Male'&!is.na(pop4$WAGP)&pop4$WAGP>=10000&pop4$abbr%in%c('CA','NY'),] #filter the people we need
sugardaddyNYCA2<-aggregate(sugardaddyNYCA$PWGTP,by=list(sugardaddyNYCA$abbr,sugardaddyNYCA$NAICS),FUN=sum)
names(sugardaddyNYCA2)<-c('State','Industry','CountWithWeight')
```

<p>Generate a table of the industries they work for NY</p>

``` r 
sugardaddyNY<-sugardaddyNYCA4[sugardaddyNYCA4$State=='NY',]
sugardaddyNY<-sugardaddyNY[rev(order(sugardaddyNY$CountWithWeight)),]
head(sugardaddyNY[,c('State','Industry','CountWithWeight','Perc')])
```

<p>Do the same thing for CA</p>

``` r 
sugardaddyCA<-sugardaddyNYCA4[sugardaddyNYCA4$State=='CA',]
sugardaddyCA<-sugardaddyCA[rev(order(sugardaddyCA$CountWithWeight)),]
head(sugardaddyCA[,c('State','Industry','CountWithWeight','Perc')])
```

    ##    State             Industry CountWithWeight Perc
    ## 26    NY Professional Service          247305 1.26
    ## 30    NY               Retail          218306 1.11
    ## 25    NY        Entertainment          212229 1.08
    ## 27    NY              Finance          193745 0.99
    ## 22    NY         Construction          171875 0.87
    ## 31    NY        Manufacturing          145781 0.74

    ##    State             Industry CountWithWeight Perc
    ## 15    CA               Retail          478291 1.25
    ## 12    CA Professional Service          464896 1.21
    ## 11    CA        Entertainment          396080 1.03
    ## 4     CA        Manufacturing          346435 0.90
    ## 7     CA         Construction          313626 0.82
    ## 13    CA              Finance          217730 0.57

<p>From the table above, the top 3 industries are retail, professional services and manufacturing industry. To hunt my sugar daddy, I will have higher chance if I can work in these industries in either New York or California</p>

# 5 Example for hunting dream ladies
=======================================

<p>Imagine I am a young man whose girlfriend criterion are:</p>

<i>"she will be younger than 30 years old with at least a university degree and she must have a job"</i>

<p>Then I have the following plots:</p>

``` r
pgirl<-pop3[pop3$single=='Single'&pop3$SEX==2&pop3$AGEP<30&!is.na(pop3$SCHL)&pop3$SCHL>=21&!is.na(pop3$ESR)&pop3$ESR%in%c(1,2,4,5),]
pgirl2<-aggregate(pgirl$PWGTP,by=list(pgirl$abbr), FUN=sum)
names(pgirl2)<-c('State','CountWithWeight')
pgirl3<-merge(pgirl2,state,by='State',all.x=T)
pgirl3$Perc<-round(pgirl3$Count/pgirl3$TotalCount*100,1)
pgirl4<-merge(pgirl3,actualPopulation,by='StateName',all.x=T)
pgirl4$ExpectedCount2015<-round(pgirl4$Perc*pgirl4$ActualPop/100,0)

ichoropleth(Perc ~ State,data=pgirl4,pal = 'PuRd')
```

<b>Expected percentage of single females younger between 16-29 years old, graduated from college, and have a job</b>

<p align="left"><img src="doc/image/perfectGirlPerc.png" ></p>

``` r 
ichoropleth(ExpectedCount2015 ~ State,data=pgirl4,pal = 'PuRd')
```

<b>Expected total number of single females younger between 16-29 years old, graduated from college, and have a job</b>

<p align="left"><img src="doc/image/perfectGirlNum.png" ></p>

<p>New York, California and Massachuset are great choice for me and I have more options such as Pennsylvania, Illinois and Nevada</p>

# 6 Conclusions
==========================

<p>As a master student, if you are single then you should be aware that to find a life partner who has similar qualification and income level as you is really hard after you leave school. So try to find one at school!

If you are a rich gentleman who do not care your wife's income, then obviously you have more choice. You do not need to rush. Girls will do their best to find you! </p>

# 7 Next Step
=============

<p>I would like to build a search engine that automate the process of filtering and plotting and it will show the best states to find your life partner based on your preference</p>

<p>Below is a demo, hope you like it! </p>

<a href="https://yueshengu.shinyapps.io/findingLifePartner/" target="_blank" style="font-size:40px">Soulmate Exploration Engine</a> \`\`\`
