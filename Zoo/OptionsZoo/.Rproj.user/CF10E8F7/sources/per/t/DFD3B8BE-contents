library(shiny)
library(bslib)
{
    library(derivmkts)
}

# Define server logic ----
server <- function(input, output) {
    output$distPlot <- renderPlot({
        price <- input$price
        strike <- input$strike
        vol <- input$vol
        tt <- input$tt
        range <- input$range
        x_range <- seq(range[1], range[2], length.out=100)
        profit <- bscall(x_range, strike, vol, 0, tt/365, 0)
        df <- data.frame(Price=x_range, Profit=profit)
        ggplot(df) + geom_line(aes(Price, Profit))
    })
}


# Define UI ----
ui <- pageWithSidebar(
    headerPanel('Iris k-means clustering'),
    sidebarPanel(
        selectInput('strategy', 'Strategy', c("Call", "Put")),
        numericInput('price', 'Asset Price', 100, min = 0),
        numericInput('strike', 'Strike', 100, min = 0),
        numericInput('vol', 'Implied Volatility', 0.3, min = 0, step = 0.01),
        numericInput('tt', 'Time to expiration', 100, min = 0),
        numericInput('rates', 'Interest Rates', 0, min = 0),
        numericInput('dividend', 'Dividends', 0, min = 0),
        sliderInput("range", "Range", 0, 200, value = c(50, 150), sep = "")
    ),
    mainPanel(
        plotOutput('distPlot')
    )
)



# Run the app ----
shinyApp(ui = ui, server = server)


