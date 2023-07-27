library(shiny)
library(SummarizedExperiment)

source("circ_dendo_plotter.R") #assuming circ_dendo_plotter function is defined in this file

ui <- fluidPage(
    titlePanel("Circular Dendrogram Plotter"),

    sidebarLayout(
        sidebarPanel(
            textInput("assay_choice", "Assay Choice", value = "logdat"),
            textInput("batchname", "Batch Name", value = "batch"),
            textInput("conditions", "Conditions", value = "condition"),
            actionButton("submit", "Submit")
        ),

        mainPanel(
            plotOutput("dendrogramPlot")
        )
    )
)

server <- function(input, output, session) {

    plot_data <- eventReactive(input$submit, {
        se_object <- readRDS("data/signature_data.RDS")
        circ_dendo_plotter(se_object = se_object,
                           assay_choice = input$assay_choice,
                           batchname = input$batchname,
                           conditions = strsplit(input$conditions, ",")[[1]])
    })

    output$dendrogramPlot <- renderPlot({
        plot_data()
    })
}

shinyApp(ui = ui, server = server)
