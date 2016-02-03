dbHeader<-dashboardHeader(title='Soulmate Explorer')

dashboardPage(

    skin="red",
  
  dbHeader,

  dashboardSidebar(
    h4('Options:',align='center'),
    selectInput("gender","Gender:",c('Male','Female'),selected='Male',multiple=F,width="100%"),
    selectInput("singleStatus","Single Status:",c('Never married','Divorced','Widowed','Separated'),
                selected='Never married',multiple=T,width="100%"),
    sliderInput('ageInput','Age Range:',15,95,c(20,30))
    ),
              
  dashboardBody(
    
               fluidRow(
                 column(width=12,infoBoxOutput("bestOption")),
                 showOutput("chart1", "datamaps")
                 
                 )
  )
)