library(shiny)
countries<-read.csv("data/countries.csv", header=TRUE, stringsAsFactors = FALSE)
indicators<-read.csv("data/indicators.csv", header=TRUE, stringsAsFactors = FALSE)

shinyUI(pageWithSidebar(
        headerPanel('About our world 1961-2012'),
               
        sidebarPanel(                
                selectInput('Country', 'Country', countries$Country, selected="World",selectize = FALSE),
                br(),
                br(),
                selectInput('toPlot', 'Indicator', indicators$choices, selected="Total population",selectize = FALSE),
                br(),
                sliderInput(inputId = "yearStartEnd",label = "Period",                            
                    min = 1961, max = 2012, step = 1, value = c(1961,2012),format = "####")
                ),
        
        mainPanel(
                tabsetPanel(type="tab",
                            tabPanel("App",
                                     h3(textOutput("textCountry")),
                                     h4(textOutput("text1")),
                                     br(),
                                     h5("Calculations"),
                                     textOutput("oldestData"),
                                     br(),
                                     textOutput("youngestData"),
                                     br(),
                                     textOutput("change"),
                                     br(),
                                     h5("Plot"),
                                     textOutput("plotText"),
                                     plotOutput("plot1"),
                                     br(),
                                     h5(textOutput("table1Text")),
                                     tableOutput("table1"),
                                     h5(textOutput("table2Text")),
                                     tableOutput("table2")
                                     ),
                            tabPanel("About",
                                     includeText("data/aText1.txt"),
                                     br(),
                                     br(),
                                     includeText("data/aText2.txt"),
                                     br()
                                    ),
                            tabPanel("Help",
                                     pre(includeText("data/hText1.txt")),
                                     br()
                                     ),
                            tabPanel("Data",
                                     pre(includeText("data/dText1.txt")),
                                     br()
                                     )
                )
        )
)
)
