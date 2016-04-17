library(shiny)
library(rCharts)
library(dplyr)
library(googleVis)

StormData<-read.csv("data/StormData.csv")

eventTypes<-sort(unique(as.character(StormData$EVTYPE)))
states<-sort(unique(as.character(StormData$STATE)))
DataByYear<- function(data, yearStart, yearEnd, events){
        data %>% filter(YEAR>=yearStart, YEAR<=yearEnd, EVTYPE %in% events) %>% 
        group_by(YEAR) %>% summarise_each(funs(sum), Frequency:PropertyDamage)
}

DataByState<- function(data, yearStart, yearEnd, events){
        data %>% filter(YEAR>=yearStart, YEAR<=yearEnd, EVTYPE %in% events) %>% 
        group_by(STATE) %>% summarise_each(funs(sum), Frequency:PropertyDamage)
}

PredictionData<- function(data, yearStart, yearEnd, events, ST){
        data %>% filter(YEAR>=yearStart, YEAR<=yearEnd, EVTYPE %in% events, STATE %in% ST) %>%
        group_by(YEAR) %>% summarise_each(funs(sum), Frequency:PropertyDamage)
}

ui<-fluidPage(
    headerPanel("Storm Database Explorer"),
    sidebarPanel(
        p("Use this web app to explore the US National Oceanic and Atmospheric Asministration's storm database. 
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
            ),
            tabPanel(("Storm predictor"),
                     h5(""),
                     p("Predict the effects of any or all storm types, in any or all states, for any year. Simply select
                        the year range you want to base the prediction over and the types of storms you want to predict 
                        on the left. Then enter in which year and what state you want to predict for below."),
                     numericInput("predYr", "Year to predict", min="2016", max="2050",value="2016"),
                     radioButtons('stateRadio','State selection type:',list('All','Manual'),selected='All'),
                     conditionalPanel(condition = "input.stateRadio != 'All'",selectInput('state', 'Choose state:',
                                                                                          states)),
                     h4("Predicted number of deaths"),
                     verbatimTextOutput("predictionDeaths"),
                     h4("Predicted number of injuries"),
                     verbatimTextOutput("predictionInjuries"),
                     h4("Predicted cost of crop damage"),
                     verbatimTextOutput("predictionCropDamage"),
                     h4("Predicted cost of property damage"),
                     verbatimTextOutput("predictionPropertyDamage"),
                     br(),
                     h5("Notes about the predictions:"),
                     p("These predictions should only be interpreted as a rough guide. The prediction algorithm uses the
                        'lm' method, which typically only explains about 30-40% of the variability in this data. In some 
                        cases, the way the data was collected varied by year, leading to some odd trends which the 
                        prediction algorithm can struggle with. The algorithm will throw up an error when there is no 
                       data - try searching for tsunami events in Kansas (KS)!")
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
        observe({
            if(input$stateRadio == "All"){
                value$selcStates<-states
            }
            else{
                value$selcStates<-input$state
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
        output$predictionDeaths<-renderPrint({
            data7<-PredictionData(StormData,input$yearRange[1],input$yearRange[2],value$selc,value$selcStates)
            fit<-lm(Deaths~YEAR,data=data7)
            year<-data.frame(YEAR=input$predYr)
            if(predict(fit,newdata=year)<0){prediction<-0}else{prediction<-predict(fit,newdata=year)}
            cat(paste("The prediction is ",round(prediction,digits = 0)," deaths.",sep=""))
        })
        output$predictionInjuries<-renderPrint({
            data8<-PredictionData(StormData,input$yearRange[1],input$yearRange[2],value$selc,value$selcStates)
            fit<-lm(Injuries~YEAR,data=data8)
            year<-data.frame(YEAR=input$predYr)
            if(predict(fit,newdata=year)<0){prediction<-0}else{prediction<-predict(fit,newdata=year)}
            cat(paste("The prediction is ",round(prediction,digits = 0)," injuries.",sep=""))
        })
        output$predictionCropDamage<-renderPrint({
            data9<-PredictionData(StormData,input$yearRange[1],input$yearRange[2],value$selc,value$selcStates)
            fit<-lm(CropDamage~YEAR,data=data9)
            year<-data.frame(YEAR=input$predYr)
            if(predict(fit,newdata=year)<0){prediction<-0}else{prediction<-predict(fit,newdata=year)}
            cat(paste("The prediction is $",round(prediction,digits = 0)," crop damage.",sep=""))
        })
        output$predictionPropertyDamage<-renderPrint({
            data10<-PredictionData(StormData,input$yearRange[1],input$yearRange[2],value$selc,value$selcStates)
            fit<-lm(PropertyDamage~YEAR,data=data10)
            year<-data.frame(YEAR=input$predYr)
            if(predict(fit,newdata=year)<0){prediction<-0}else{prediction<-predict(fit,newdata=year)}
            cat(paste("The prediction is $",round(prediction,digits = 0)," property damage.",sep=""))
        })
    }
)

shinyApp(ui=ui,server = server)
