#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    
    output$other_plot <- renderPlot({
        df <- dataset()
        req(nrow(df) > 0)
        
        # Example other plot: histogram of avgOptVolu20d
        ggplot(df, aes(x = avgOptVolu20d)) +
            geom_histogram(bins = 30, fill = "steelblue", color = "white") +
            labs(title = "Histogram of avgOptVolu20d", x = "avgOptVolu20d", y = "Count") +
            theme_minimal()
    })

}
