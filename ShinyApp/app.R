library(shiny)
library(rCharts)
library(dplyr)
library(googleVis)

StormData<-read.csv("data/StormData.csv")

eventTypes<-unique(as.character(StormData$EVTYPE))
DataByYear<- function(data, yearStart, yearEnd, events){
        data %>% filter(YEAR>=yearStart, YEAR<=yearEnd, EVTYPE %in% events) %>% 
        group_by(YEAR) %>% summarise_each(funs(sum), Frequency:PropertyDamage)
}

DataByState<- function(data, yearStart, yearEnd, events){
        data %>% filter(YEAR>=yearStart, YEAR<=yearEnd, EVTYPE %in% events) %>% 
        group_by(STATE) %>% summarise_each(funs(sum), Frequency:PropertyDamage)
}

ui<-fluidPage(
    headerPanel("Storm Database Explorer"),
    sidebarPanel(
        p("Use this webapp to explore the US National Oceanic and Atmospheric Asministration's storm database. 
          You can select a year range and storm types below."),
        p("The source code for processing the database and for this webapp is available on my ",
          a("github.", href="https://github.com/aem56/StormsAnalysis")),
        sliderInput("yearRange","Year Range:",min=1950,max=2011,value = c(1990,2011)),
        radioButtons('radio','Storm selection type:',list('All','Manual'),selected='All'),
        conditionalPanel(condition = "input.radio != 'All'",selectInput('evntTypes', 'Storm types:', eventTypes))
    ),
    mainPanel(
        tabsetPanel(
            tabPanel(("By year"),
                h4("Number of recorded storm events by year:"),
                showOutput("plot1",'morris'),
                h4("Population impact by year:"),
                showOutput("plot2", 'morris'),
                h4("Economic impact by year:"),
                showOutput("plot3",'morris')
            ),
            tabPanel(("By state"),
                     h4("Number of recorded storm events by state:"),
                     htmlOutput("plot4"),
                     h4("Population impact by state:"),
                     htmlOutput("plot5"),
                     h4("Economic impact by state:"),
                     htmlOutput("plot6")
            )
        )
    )
)

server<-shinyServer(
    function(input,output){
        value<-reactiveValues()
        value$selc<-eventTypes
        observe({
            if(input$radio == "All"){
                value$selc<-eventTypes
            }
            else{
                value$selc<-input$evntTypes
            }
        })
        output$plot1<-renderChart2({
            data1<-DataByYear(StormData,input$yearRange[1],input$yearRange[2], value$selc)
            data1$YEAR<-as.character(data1$YEAR)
            mPlot(x="YEAR",y="Frequency",data=data1,type="Line")
        })
        output$plot2<-renderChart2({
            data2<-DataByYear(StormData,input$yearRange[1],input$yearRange[2], value$selc)
            data2$YEAR<-as.character(data2$YEAR)
            mPlot(x="YEAR",y=c("Injuries","Deaths"),data=data2,type="Line")
        })
        output$plot3<-renderChart2({
            data3<-DataByYear(StormData,input$yearRange[1],input$yearRange[2], value$selc)
            data3$YEAR<-as.character(data3$YEAR)
            mPlot(x="YEAR", y=c("PropertyDamage","CropDamage"), data=data3, type="Line")
        })
        output$plot4<-renderGvis({
            data4<-DataByState(StormData,input$yearRange[1],input$yearRange[2], value$selc)
            gvisGeoChart(data4, "STATE", "Frequency",options=list(region="US", displayMode="regions",
                        resolution="provinces",width=600, height=400))
        })
        output$plot5<-renderGvis({
            data5<-DataByState(StormData,input$yearRange[1],input$yearRange[2], value$selc)
            gvisGeoChart(data5, "STATE", "Deaths","Injuries",options=list(region="US", displayMode="regions", 
                        resolution="provinces",width=600, height=400))
        })
        output$plot6<-renderGvis({
            data6<-DataByState(StormData,input$yearRange[1],input$yearRange[2], value$selc)
            gvisGeoChart(data6, "STATE", "PropertyDamage","CropDamage",options=list(region="US", displayMode="regions", 
                        resolution="provinces",width=600, height=400))
        })
    }
)

shinyApp(ui=ui,server = server)
