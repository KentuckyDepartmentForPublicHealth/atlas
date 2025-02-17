library(shiny)
library(plotly)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("t-SNE Dimensionality Reduction"),
  plotlyOutput("tsnePlot"),
  DT::dataTableOutput("dataTable")
)

server <- function(input, output) {
  # Ensure 'atlasDataClean' has a 'key' column
  if(!"key" %in% names(atlasDataClean)){
    atlasDataClean$key <- seq_len(nrow(atlasDataClean))
  }
  
  filteredData <- reactive({
    atlasDataClean
  })
  
  output$tsnePlot <- renderPlotly({
    data <- filteredData()
    validate(need(nrow(data) > 0, "No data available to plot."))
    
    # Ensure 'diagnosisClass' and 'diagnosisFinal' are factors and drop unused levels
    data <- data %>%
      mutate(
        diagnosisClass = as.factor(diagnosisClass),
        diagnosisFinal = as.factor(diagnosisFinal)
      )
    data$diagnosisClass <- droplevels(data$diagnosisClass)
    data$diagnosisFinal <- droplevels(data$diagnosisFinal)
    
    # Create a custom color palette for 'diagnosisFinal'
    diagnosisFinal_levels <- levels(data$diagnosisFinal)
    num_diagnosisFinal <- length(diagnosisFinal_levels)
    custom_colors <- setNames(rainbow(num_diagnosisFinal), diagnosisFinal_levels)
    
    # Calculate centroids for each diagnosisFinal
    centroids <- data %>%
      group_by(diagnosisFinal) %>%
      summarise(tsne1 = median(tsne1), tsne2 = median(tsne2))
    
    # Initialize the Plotly object
    p <- plot_ly(source = "A")
    
    # Loop over 'diagnosisClass' to create grouped traces
    for (diag_class in levels(data$diagnosisClass)) {
      class_data <- data %>% filter(diagnosisClass == diag_class)
      
      if (nrow(class_data) == 0) next
      
      # Drop unused levels in class_data$diagnosisFinal
      class_data$diagnosisFinal <- droplevels(class_data$diagnosisFinal)
      
      diag_finals_in_class <- levels(class_data$diagnosisFinal)
      
      group_added <- FALSE
      
      for (diag_final in diag_finals_in_class) {
        diag_data <- class_data %>% filter(diagnosisFinal == diag_final)
        
        if (nrow(diag_data) == 0) next
        
        # Set legendgrouptitle only on first trace in the group
        if (!group_added) {
          group_added <- TRUE
          legendgrouptitle = list(text = diag_class)
        } else {
          legendgrouptitle = NULL
        }
        
        p <- add_trace(
          p,
          data = diag_data,
          x = ~tsne1,
          y = ~tsne2,
          type = 'scatter',
          mode = 'markers',
          name = diag_final,
          legendgroup = diag_class,
          showlegend = TRUE,
          legendgrouptitle = legendgrouptitle,
          marker = list(size = 5, opacity = 0.8, color = custom_colors[diag_final]),
          text = ~paste("Sample:", sampleID,
                        "<br>Diagnosis Class:", diagnosisClass,
                        "<br>Diagnosis Final:", diagnosisFinal),
          hoverinfo = 'text',
          key = ~key
        )
      }
    }
    
    # Add annotations for centroids
    p <- layout(
      p,
      title = "t-SNE Dimensionality Reduction",
      xaxis = list(title = "t-SNE 1", color = 'white'),
      yaxis = list(title = "t-SNE 2", color = 'white'),
      plot_bgcolor = '#111111',
      paper_bgcolor = '#111111',
      font = list(color = 'white'),
      dragmode = 'select',
      legend = list(
        itemclick = "toggleothers",
        itemdoubleclick = FALSE
      ),
      annotations = lapply(1:nrow(centroids), function(i) {
        list(
          x = centroids$tsne1[i],
          y = centroids$tsne2[i],
          text = centroids$diagnosisFinal[i],
          showarrow = FALSE,
          font = list(size = 10, color = 'white')
        )
      })
    )
    
    # Register the selection event
    p <- event_register(p, "plotly_selected")
    
    return(p)
  })
  
  selected_points <- reactive({
    event_data("plotly_selected", source = "A")
  })
  
  output$dataTable <- DT::renderDataTable({
    select_data <- selected_points()
    if (!is.null(select_data) && nrow(select_data) > 0) {
      point_keys <- as.numeric(select_data$key)
      selected_data <- atlasDataClean[atlasDataClean$key %in% point_keys, ]
      if(nrow(selected_data) > 0) {
        return(DT::datatable(selected_data))
      } else {
        return(DT::datatable(data.frame(Message = "No valid points selected")))
      }
    } else {
      return(DT::datatable(data.frame(Message = "No points selected")))
    }
  })
}

shinyApp(ui, server)