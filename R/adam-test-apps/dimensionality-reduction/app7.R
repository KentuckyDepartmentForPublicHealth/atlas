library(shiny)
library(plotly)
library(dplyr)
library(DT)

# Define tumor colors
tumor_colors <- c(
  "#6a0033", "#e4016e", "#ff527b", "#360e15", "#ff757c", "#dc002d", 
  "#ff9170", "#7a2000", "#ff7e40", "#ac5300", "#da6c00", "#401f00",
  "#ffb173", "#d79600", "#6a4800", "#d6c6b2", "#ddaf00", "#d6ca6f",
  "#6e6900", "#1d1c10", "#8db600", "#1b7a00", "#8ddb76", "#00b861",
  "#9ad5b1", "#005d3f", "#01bbb7", "#00444d", "#01afc7", "#54d8f9",
  "#0189dd", "#8ab5ff", "#5292ff", "#004690", "#00317f", "#4263ff",
  "#240a4e", "#271332", "#fa63ff", "#760078", "#ff77f6", "#8a005f",
  "#ffa5ca"
)

ui <- fluidPage(
  titlePanel("t-SNE Dimensionality Reduction"),
  plotlyOutput("tsnePlot"),
  DT::dataTableOutput("dataTable")
)

server <- function(input, output) {
  # Add key column if it doesn't exist
  if(!"key" %in% names(atlasDataClean)) {
    atlasDataClean$key <- seq_len(nrow(atlasDataClean))
  }
  
  filteredData <- reactive({
    atlasDataClean
  })
  
  output$tsnePlot <- renderPlotly({
    data <- filteredData()
    validate(need(nrow(data) > 0, "No data available to plot."))
    
    # Convert to factors and drop unused levels
    data$diagnosisClass <- factor(data$diagnosisClass)
    data$diagnosisFinal <- factor(data$diagnosisFinal)
    
    # Split data into tumor and non-tumor
    non_tumor_data <- data[data$diagnosisClass == "NON-TUMOR", ]
    tumor_data <- data[data$diagnosisClass != "NON-TUMOR", ]
    
    # Create color mappings
    non_tumor_diagnoses <- levels(factor(non_tumor_data$diagnosisFinal))
    tumor_diagnoses <- levels(factor(tumor_data$diagnosisFinal))
    
    # Generate grey colors for non-tumor
    non_tumor_colors <- if(length(non_tumor_diagnoses) > 0) {
      grey_palette <- gray.colors(length(non_tumor_diagnoses), start = 0.2, end = 0.8)
      setNames(grey_palette, non_tumor_diagnoses)
    } else {
      NULL
    }
    
    # Use preset colors for tumor
    tumor_color_mapping <- if(length(tumor_diagnoses) > 0) {
      setNames(tumor_colors[1:length(tumor_diagnoses)], tumor_diagnoses)
    } else {
      NULL
    }
    
    # Combine color mappings
    all_colors <- c(non_tumor_colors, tumor_color_mapping)
    
    # Calculate centroids
    centroids <- data %>%
      group_by(diagnosisFinal) %>%
      summarise(tsne1 = median(tsne1), tsne2 = median(tsne2))
    
    # Create plot
    p <- plot_ly(source = "A")
    
    # Add traces for each diagnosis class
    for(class in levels(data$diagnosisClass)) {
      class_data <- data[data$diagnosisClass == class, ]
      
      if(nrow(class_data) == 0) next
      
      for(diagnosis in levels(factor(class_data$diagnosisFinal))) {
        diag_data <- class_data[class_data$diagnosisFinal == diagnosis, ]
        
        if(nrow(diag_data) == 0) next
        
        p <- add_trace(
          p,
          data = diag_data,
          x = ~tsne1,
          y = ~tsne2,
          type = 'scatter',
          mode = 'markers',
          name = diagnosis,
          legendgroup = class,
          showlegend = TRUE,
          legendgrouptitle = if(diagnosis == levels(factor(class_data$diagnosisFinal))[1]) 
            list(text = class) else NULL,
          marker = list(
            size = 5,
            opacity = 0.8,
            color = all_colors[diagnosis]
          ),
          text = ~paste(
            "Sample:", sampleID,
            "<br>Diagnosis Class:", diagnosisClass,
            "<br>Diagnosis Final:", diagnosisFinal
          ),
          hoverinfo = 'text',
          key = ~key
        )
      }
    }
    
    # Add layout
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
      }
      return(DT::datatable(data.frame(Message = "No valid points selected")))
    }
    return(DT::datatable(data.frame(Message = "No points selected")))
  })
}

shinyApp(ui, server)