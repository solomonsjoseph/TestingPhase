### DENDROGRAM TAB ###

## Plot dendrogram
observeEvent(input$dend_plot, {
  req(reactivevalue$se)
  output$dendrogram <- renderPlot({
    plot(dendrogram_plotter(reactivevalue$se,
                            input$dend_assay_name,
                            input$dend_batch_to_display,
                            input$dend_category_to_display)$dendrogram)
  }, height = function() {session$clientData$output_dendrogram_width
  })
  
  output$circular_dendrogram <- renderPlot({
    plot(dendrogram_plotter(reactivevalue$se,
                            input$dend_assay_name,
                            input$dend_batch_to_display,
                            input$dend_category_to_display)$circular_dendrogram)
  }, height = function() {session$clientData$output_circular_dendrogram_width
  })
})
