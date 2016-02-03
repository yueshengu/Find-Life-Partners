options(shiny.maxRequestSize=50*1024^2)

shinyServer(function(input, output, session) {

    mtdRev<-reactive({
      data<-pop4[pop4$MSP%in%input$singleStatus&pop4$SEX==input$gender&pop4$AGEP>=input$ageInput[1]&
                   pop4$AGEP<=input$ageInput[2],]
      
      data2<-aggregate(data$PWGTP,by=list(data$abbr), FUN=sum)
      names(data2)<-c('State','CountWithWeight')
      data3<-merge(data2,state,by='State',all.x=T)
      data3$Perc<-round(data3$Count/data3$TotalCount*100,1)
      data4<-merge(data3,actualPopulation,by='StateName',all.x=T)
      data4$ExpectedCount2015<-round(data4$Perc*data4$ActualPop/100,0)
      
      return(data4)
    })
    
    output$chart1<-renderChart2({
      if(input$gender=='Male'){
          choro<-ichoropleth(Perc ~ State,legend=F,pal='YlOrRd',data=mtdRev())
          choro$set(geographyConfig = list(
            popupTemplate = "#! function(geography, data){
            return '<div class=hoverinfo><strong>' + geography.properties.name + 
            ': ' + data.Perc + '% of state population are single male' + '</strong></div>';
        } !#" 
          ))
    }else{
      
        choro<-ichoropleth(Perc ~ State,legend=F,pal='PuRd',data=mtdRev())
        choro$set(geographyConfig = list(
          popupTemplate = "#! function(geography, data){
          return '<div class=hoverinfo><strong>' + geography.properties.name + 
          ': ' + data.Perc + '% of state population are single female' + '</strong></div>';
      } !#" 
        ))
      
    }
      choro
    })
    
    output$bestOption<-renderInfoBox({
      data<-mtdRev()
      if(input$gender=='Male'){
        #browser()
        
          return(infoBox(
            paste0('Best State is'),
            paste0(data[rev(order(data$Perc)),'StateName'][1],' : ',max(data$Perc),
                   "% of state population are single male"),
            icon=icon("male"),color="red"
          ))
        
      }else{
        
          return(infoBox(
            paste0('Best State is'),
            paste0(data[rev(order(data$Perc)),'StateName'][1],' : ',max(data$Perc),
                   "% of state population are single female"),
            icon=icon("female"),color="purple"
          ))
        
      }
    })

  })