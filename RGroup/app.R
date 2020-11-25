library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(DT)
library(dplyr)


ui<- dashboardPage(
    dashboardHeader(title = "Crime Analytics"),
    dashboardSidebar(
        selectInput(inputId ="classes_input", label="Select Class:", choices=c("CRIMES_AGAINST_THE _PERSON", "CONTACT_RELATED_CRIMES", "PROPERTY_RELATED_CRIMES", "OTHER_SERIOUS_CRIMES", "CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION", "SUBCATEGORIES_OF_AGGRAVATED_ROBBERY")), 
        
        sliderInput('slider','Select number of cases:',min= 4, max = 400, value = 0)
    ),
    dashboardBody(
        column(width = 6,
               plotOutput("Class_crime", height = 400)
        ),
        column(width = 6,
               plotOutput("PlotTCrimes", height = 400)
        ),
        DT::dataTableOutput("table", width = "100%", height = "auto")
    )
    
)
server <- function(input, output){
    
    mycsv<-read.csv("crime_data.csv")
    
    mycsv_1<-melt(wdf2,id.vars=c("classes","CRIME_CATEGORY"), measure.vars = c("April_2006_to_March_2007","April_2007_to_March_2008","April_2008_to_March_2009","April_2009_to_March_2010","April_2010_to_March_2011","April_2011_to_March_2012","April_2012_to_March_2013","April_2013_to_March_2014","April_2014_to_March_2015","April_2015_to_March_2016"),
                  variable.name="Period") 
    
    df_1<- aggregate(value~Period+classes,mycsv_1,sum)
    df_2<- aggregate(value~Period,sum) 
    mycsv_1<- na.omit(mycsv_1) 
    
    d_c <- reactive({
        if(input$selectcrime_case=="CRIMES_AGAINST_THE_PERSON"){
            mycsv_2 <- mycsv %>% filter(classes==input$selectcrime_case)
        }
        else if (input$selectcrime_case== "CONTACT_RELATED_CRIMES"){
            mycsv_2 <- mycsv %>% filter(classes==input$selectcrime_case)
        }
        else if (input$selectcrime_case== "PROPERTY_RELATED_CRIMES"){
            mycsv_2 <- mycsv %>% filter(classes==input$selectcrime_case)
        }
        
        else if (input$selectcrime_case== "OTHER_SERIOUS_CRIMES"){
            mycsv_2 <- mycsv %>% filter(classes==input$selectcrime_case)
        }
        else if (input$selectcrime_case== "CRIME_DETECTED_AS_A_RESULT_OF_POLICE_ACTION"){
            mycsv_2 <- mycsv %>% filter(classes==input$selectcrime_case)
        }
        else{
            mycsv_2 <- mycsv %>% filter(classes==input$selectcrime_case)
        }
        return(mycsv_2)
    })
    
    
    
    
    output$CrimesPerClass<-renderPlot({
        data_classes() %>%
            ggplot(aes(x=reorder(Period,value), y=value, fill=reorder(Period,value)))+
            geom_col()+
            labs(x=element_blank(),y="Total Crimes",title = "Crimes results per Class")+
            guides(fill=FALSE)+
            theme_bw()+
            theme(plot.title = element_text()+
                      coord_flip()
                  
    })
        
        output$PlotTCrimes <- renderPlot({
            totalCrimes() %>% 
                ggplot(aes(x=reorder(Period,value), y=value, fill=reorder(Period,value)))+
                geom_col()+
                labs(x=element_blank(), y="Cases in a period", title = "Total number ofCrime Cases")+
                guides(fill= FALSE)+
                theme_bw()+
                theme(plot.title = element_text())+
                coord_flip()
        })
        output$ot = DT::renderDataTable({
            ot() 
        })
}  

shinyApp(ui= ui, server = server)

