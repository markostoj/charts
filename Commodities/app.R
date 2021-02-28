# library(shiny)
library(shinyWidgets)
library(dslabs)
library(tidyverse)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)

#to publish - setwd("E:/Programming/RStudio/Commodities")

commodity <- data.frame(read.csv("commodities.csv", header=T))
index <- data.frame(read.csv("index.csv", header=T))

commodity$Date <-as.Date(commodity$Date, format = "%m/%d/%Y")
index$Date <-as.Date(index$Date, format = "%m/%d/%Y")

commodity <- mutate(commodity, Year=as.numeric(format(Date,'%Y'))) %>%
        pivot_longer(cols = c(Value, Volume), 
                     names_to = "data", values_to = "value")

index <- mutate(index, Year=as.numeric(format(Date,'%Y')))

head(commodity)

ui <- fluidPage(
        
        titlePanel("Commodity prices and S&P500 over time"),
        sidebarLayout(
                sidebarPanel(
                        # inputs
                        
                        sliderInput("yearInput", "Year", min=1980, max=2020, 
                                    value=c(1995, 2020), sep=""),
                        
                        radioGroupButtons("dataInput", "data",
                                          choiceNames = list("Price", "Volume"),
                                          choiceValues = list("Value", "Volume")),
                        
                        checkboxGroupInput("commodityInput", "Commodity",
                                           choices = c("Gold","Silver","Platinum","Palladium","Copper","Aluminum","Zinc","Steel","Crude Oil","Natural Gas","Gasoline","Heating Oil","Ethanol","Kerosene","Coal","Uranium","Electricity","Carbon Emissions","Rubber ","Lumber","Terephthalic Acid","Plastics","Cotton","Greasy Wool ","Cor","Whea","Oat","Rough Rice","Soybea","Azuki Red Beans ","Canola","Turmeric","Jeera","Sorghum","Sunflowers","Sugar","Cocoa","Coffee","Milk","Butter","Orange Juice","Palm Oil ","Cheese","Coriander","Shrimp","Whey","Lean Hogs","Feeder Cattle"),
                                           selected = c("Gold", "Silver"))
                        
                ),  
                
                mainPanel(
                        plotOutput("indexplot"),
                        br(), br(),
                        plotOutput("commodityplot"),
                        br(), br(),
                        plotlyOutput("distplot")
                        
                        
                ) 
        )   
)   

server <- function(input, output) {
        
        d <- reactive({
                commodity %>%
                        filter(Commodity %in% input$commodityInput,
                               Year >= input$yearInput[1],
                               Year <= input$yearInput[2],
                               data == input$dataInput
                        )
        }) 
        
        d2 <- reactive({
                index %>%
                        filter(
                               Year >= input$yearInput[1],
                               Year <= input$yearInput[2]
                        )
        }) 
        
        output$commodityplot <- renderPlot({
                
                ggplot(d(), aes(x=Date, y = value, color = Commodity)) +
                        geom_line() + 
                        ylab(input$dataInput) +
                        ggtitle("USD per unit over time")+
                        scale_fill_continuous(guide = guide_legend()) +
                        theme(legend.position="bottom")
        })
 
        output$distplot <- renderPlotly({
                
                box <- plot_ly(d(), y = ~value,
                               color = ~Commodity, type = "box")  %>%
                        layout(title = "Distribution of prices over different years",
                        yaxis = list(title=input$dataInput))
                
        })
        
        output$indexplot <- renderPlot({
                
                ggplot(d2(), aes(x=Date, y = Close)) +
                        geom_line() +
                        ggtitle("S&P500")
        })
        
}

shinyApp(ui=ui, server=server)