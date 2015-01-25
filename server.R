library(shiny)
library(reshape2)
library(graphics)
library(plyr)
dataMelt<-read.csv("data/dataMelt.csv", header=TRUE, stringsAsFactors = FALSE)
countries<-read.csv("data/countries.csv", header=TRUE, stringsAsFactors = FALSE)
indicators<-read.csv("data/indicators.csv", header=TRUE, stringsAsFactors = FALSE)


shinyServer(function(input, output, session) {

        
        
        indicatorName<-reactive({
                indicators[which(indicators$choices==input$toPlot),c("indicator")]
        })
        

        output$textCountry <- renderText({ input$Country })
        
        dt <- reactive({
                indicatorName()
        })
        
        output$text1 <- renderText({ dt() })
        
       
        subData0<-reactive({
                subset(dataMelt,dataMelt$CountryName==input$Country)
        })                            
        subData1<-reactive({
                dcast(subData0(),variable~IndicatorName)
        }) 
        plotData0<-reactive({
                cbind(Year=subData1()[,c("variable")],values=subData1()[,c(indicatorName())])
        })     
        rowsTotal<-reactive({
                nrow(plotData0())
        })
        
        index<-reactive({
                c(1:rowsTotal())
        })
        
        
        plotData1<-reactive({
                cbind(Index=index(),plotData0())
        })
        
     
        
        
        slideL<-reactive({
                input$yearStartEnd[1]
        })
        slideR<-reactive({
                input$yearStartEnd[2]
        })
        
        idxL<-reactive({
                plotData1()[plotData1()[,2]==slideL(),1]
        })
        idxR<-reactive({
                plotData1()[plotData1()[,2]==slideR(),1]
        })
        
        plotData2<-reactive({
                plotData1()[idxL():idxR(),]
        })
        
                    
        
        
        
        plotDataWONAa<-reactive({
                subset(plotData2(),plotData2()[,3]!="")
        })
        plotDataWONA<-reactive({
                subset(plotDataWONAa(),plotDataWONAa()[,3]!= 0 )
        })        
        
        rowsWONA<-reactive({
                nrow(plotDataWONA())
        })
        rowsWONAa<-reactive({
                nrow(plotDataWONAa())
        })

        
     
        
        
        
        
        startYofData<-reactive({
                if(rowsWONA()>0){
                        plotDataWONA()[1,][2]    
                }
        })
        endYofData<-reactive({
                if(rowsWONA()>0){
                        plotDataWONA()[rowsWONA(),][2]                       
                }
        })              
        oldest<-reactive({
                if(rowsWONA()>0){
                        plotDataWONA()[plotDataWONA()[,2]==startYofData(),]                        
                }
        })                
        youngest<-reactive({
                if(rowsWONA()>0){
                        plotDataWONA()[plotDataWONA()[,2]==endYofData(),]                        
                }
        })

        
        
        change<-reactive({
                if(rowsWONA()>0){
                        abs(youngest()[3]-oldest()[3])/(oldest()[3])*100                       
                }
        })
        
        period<-reactive({
                if(rowsWONA()>0){
                        youngest()[2]-oldest()[2]                       
                }
        }) 
        
        
        changeDir<-reactive({
                if(rowsWONA()>0){
                        if(youngest()[3]>oldest()[3]){
                                "increase"
                        }else{
                                "decrease"
                        }       
                }
        })

        
        

       
        TEXToldestData<-reactive({
                if(rowsWONA()>0){
                        paste("The first data point > 0 in the period is:",  round(oldest()[3],3)," from year ",oldest()[2])
                }else{
                        paste("The first data point > 0 in the period is: Sorry there is no data > 0 between the years", slideL(), "and",slideR() , ". Please adjust your input in the sidebarpanel to the left")
                }
                
        })
        TEXTyoungestData<-reactive({
                if(rowsWONA()>0){
                        paste("The last data point > 0 in the period is:",   round(youngest()[3],3)," from year ",youngest()[2])
                }else{
                        paste("The last data point > 0 in the period is: Sorry there is no data > 0 between the years", slideL(), "and",slideR() , ". Please adjust your input in the sidebarpanel to the left")
                }
                
        })
        TEXTchange<-reactive({
                if(rowsWONA()>0){
                        paste("The change from ",oldest()[2] ," to ",youngest()[2] ," gives a ",round(change(),0), "% ",changeDir()," over a period of", period()," years")
                }else{
                        ""
                }
                
        })
        
        
        
        output$oldestData <- reactive({TEXToldestData()})
                
                
        output$youngestData <- reactive({TEXTyoungestData()})
        
        
        output$change<-reactive({TEXTchange()})        
        

        plotT<-reactive({
                if(rowsWONAa()>0){
                        ""
                }else{
                        "Sorry, there is no data to plot"
                }
        })
        output$plotText<-reactive({plotT()}) 
        
   
        output$plot1<-renderPlot({
                                par(mar = c(5.1, 8, 1, 1))
                                plot(plotData2()[,2:3], pch=20,xlim=c(input$yearStartEnd),ylab="",xlab="Year",
                                     cex = 1.5,col="blue",las=1)
                                mtext(indicatorName(), side = 2, line = 6,cex=0.95)
                        })
 


        
        
        
        
        
        dateForTable<-reactive({
                if(rowsWONA()>0){
                        endYofData()
                }else{
                        slideR()
                }
        })
        
        
        subDataIndicator00<-reactive({
                subset(dataMelt,dataMelt$IndicatorName==indicatorName() &
                               dataMelt$variable==dateForTable())                
        })
        subDataIndicator01<-reactive({
                arrange(subDataIndicator00(),desc(value))               
        })
        subDataIndicator10<-reactive({
                subDataIndicator01()[,c("Region","CountryName","value")]
        })
        subDataIndicator11a<-reactive({
                subset(subDataIndicator10(),subDataIndicator10()[,3]!="")
        })
        subDataIndicator11<-reactive({
                subset(subDataIndicator11a(),subDataIndicator11a()[,3] != 0)
        })        
        
        highValue<-reactive({
                head(subDataIndicator11(),n=10)
        })
        lowValue<-reactive({
                tail(subDataIndicator11(),n=10)
        })
               
       
        
        rowsHV<-reactive({
                nrow(highValue())
        })        
        
        rowsLV<-reactive({
                nrow(lowValue())
        })
        
        sorryText2<-"Sorry, no data available"
        
        TEXTtable1<-reactive({
                if(rowsHV()>0){
                        paste("Countries with max values year ",dateForTable())
                }else{
                        paste("Sorry there is no data > 0 for year ",dateForTable())
                }
                
        })  
        TEXTtable2<-reactive({
                if(rowsLV()>0){
                        paste("Countries with min values (> 0) year ",dateForTable())
                }else{
                        paste("Sorry there is no data > 0 for year ",dateForTable())
                }
                
        }) 
        
        
        
        
        output$table1Text<-reactive({TEXTtable1()})                
        output$table1<-renderTable({
                highValue()
        }) 
        
        output$table2Text<-reactive({TEXTtable2()}) 
        output$table2<-renderTable({
                lowValue()
        })       
        
        
     
})