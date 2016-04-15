library(shiny)
library(rCharts)
library(dplyr)

StormData<-read.csv("data/StormData.csv")

eventTypes<-unique(as.character(StormData$EVTYPE))
DataByYear<- function(data, yearStart, yearEnd, events){
        data %>% filter(YEAR>=yearStart, YEAR<=yearEnd, EVTYPE %in% events) %>% 
        group_by(YEAR) %>% summarise_each(funs(sum), Frequency:PropertyDamage)
}

ui<-fluidPage(
    headerPanel("Storm Database Explorer"),
    sidebarPanel(
        p("Use this webapp to explore the US National Oceanic and Atmospheric Asministration's storm database."),
        p("The source code for processing the database and for this webapp is available on my ",
          a("github.", href="https://github.com/aem56/StormsAnalysis")),
        sliderInput("yearRange","Year Range:",min=1950,max=2011,value = c(1990,2011)),
        uiOutput("evtypeControls"),
        actionButton(inputId = "clearAll", label = "Clear selection"),
        actionButton(inputId = "selectAll", label = "Select all")
    ),
    mainPanel(
        h4("Number of recorded storm events by year:"),
        showOutput("plot1",'morris'),
        h4("Population impact by year:"),
        showOutput("plot2", 'morris'),
        h4("Economic impact by year:"),
        showOutput("plot3",'morris')
    )
)

server<-shinyServer(
    function(input,output){
        value<-reactiveValues()
        value$ET<-eventTypes
        ET<-reactive({input$evntTypes})
        output$evtypeControls <- renderUI({
            checkboxGroupInput('evntTypes', 'Storm types:', eventTypes, selected=value$ET)
        })
        observe({
            if(input$clearAll == 0) return()
            value$ET <- c()
        })
        observe({
            if(input$selectAll == 0) return()
            value$ET <- eventTypes
        })
        output$plot1<-renderChart2({
            data1<-DataByYear(StormData,input$yearRange[1],input$yearRange[2], input$evntTypes)
            data1$YEAR<-as.character(data1$YEAR)
            mPlot(x="YEAR",y="Frequency",data=data1,type="Line")
        })
        output$plot2<-renderChart2({
            data2<-DataByYear(StormData,input$yearRange[1],input$yearRange[2], input$evntTypes)
            data2$YEAR<-as.character(data2$YEAR)
            mPlot(x="YEAR",y=c("Injuries","Deaths"),data=data2,type="Line")
        })
        output$plot3<-renderChart2({
            data3<-DataByYear(StormData,input$yearRange[1],input$yearRange[2], input$evntTypes)
            data3$YEAR<-as.character(data3$YEAR)
            mPlot(x="YEAR", y=c("PropertyDamage","CropDamage"), data=data3, type="Line")
        })
    }
)

shinyApp(ui=ui,server = server)