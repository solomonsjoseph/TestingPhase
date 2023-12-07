library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$h1("Pretty toggles"),
  br(),
  
  fluidRow(
    column(
      width = 4,
      prettyToggle(                              # Best one here
        inputId = "toggle1",
        label_on = "Checked!",
        label_off = "Unchecked..."
      ),
      verbatimTextOutput(outputId = "res1"),
      br(),
      prettyToggle(                 # Best one but no suitable to what we need
        inputId = "toggle4",  label_on = "Yes!",
        label_off = "No..", outline = TRUE,
        plain = TRUE,
        icon_on = icon("thumbs-up"),
        icon_off = icon("thumbs-down")
      ),
      verbatimTextOutput(outputId = "res4")
    ),
    column(
      width = 4,
      prettyToggle(
        inputId = "toggle2",
        label_on = "Yes!", icon_on = icon("check"),
        status_on = "info", status_off = "warning",
        label_off = "No..", icon_off = icon("xmark")
      ),
      verbatimTextOutput(outputId = "res2")
    ),
    column(
      width = 4,
      prettyToggle(
        inputId = "toggle3",  label_on = "Yes!",
        label_off = "No..", shape = "round",
        fill = TRUE, value = TRUE
      ),
      verbatimTextOutput(outputId = "res3")
    )
  )
  
)

server <- function(input, output, session) {
  
  output$res1 <- renderPrint(input$toggle1)
  output$res2 <- renderPrint(input$toggle2)
  output$res3 <- renderPrint(input$toggle3)
  output$res4 <- renderPrint(input$toggle4)
  
}

if (interactive())
  shinyApp(ui, server)