
## user interface

# toplevel ----------------------------------------------------------------


ui <- page_navbar(id = 'navBar',
  theme = bs_theme(
    version = 5, bootswatch = 'default',
    primary = chfs$cols9[8], # dark mode
    secondary = chfs$cols9[2],
    success = chfs$cols9[2],
    info = chfs$cols9[4], # light mode
    light = 'red',
    warning = chfs$cols9[2],
    danger = chfs$cols9[2],
    base_font = c("Montserrat", "Helvetica", "sans-serif"),
    code_font = c("Source Code Pro", "monospace"),
    heading_font = c("Impact", "Arial", "sans-serif")

    # Can also add lower-level customization
    # "input-border-color" = chfs$cols9[4],
    # "navbar-brand-font-size" = "4rem",    # Adjust font size
    # "navbar-brand-font-family" = "Impact"    # Adjust font size
  ),
  title = span(
    # img(src = "DPH_and_PHAB_logo-removebg-preview.png", height = "75px"),
    # img(src ="icons8-atlas2-100.png", width = "100px", height = "100px"),
    img(src ="icons8-brain-tumor-100.png", width = "100px", height = "100px"),
    "Atlas of Nervous System Tumors",
    style = 'vertical-align: middle;'
  ),
  # sidebar = tagList('Toolbar',
  #   span(img(src ="icons8-brain-tumor-100.png", width = "100px", height = "100px"), style = 'text-align: center;')
  # ),
  sidebar = NULL,
  header = NULL,
  footer = NULL,
  nav_spacer(),
  

# home --------------------------------------------------------------------

  nav_panel(
    title = 'Home', icon = icon('house'),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "atlas.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    span(img(src = "DPH_and_PHAB_logo-removebg-preview.png", width = "30%"), style = 'text-align: center;'),
    h2('This is a heading'),
    tags$blockquote("This study introduces a comprehensive atlas that integrates gene expression and clinical data from thousands of nervous system samples, both neoplastic (tumorous) and non-neoplastic. It addresses the current gap in resources for studying a wide range of nervous system tumors, especially rare types across various regions and age groups. The atlas allows for in-depth comparative analysis of gene expression and reveals that patterns in DNA methylation also extend to gene expression differences across tumor types. Additionally, it identifies specific brain tumors, like certain gliomas, that need further diagnostic clarification, and the methods used can be applied to other rare diseases."),
  ),
  # nav_panel(
  #   title = "Stats", icon = icon('chart-bar'),
  #   align = "right",
  #   div(
  #     style = "margin-bottom: 30px;",  # Adds space between the value box and the footer
  #     uiOutput("my_value_box")
  #   )
  # ),
  
  
  nav_panel(
    title = "Info", icon = icon('circle-info'),
    align = "right"
  ),
  
  
  
# resources ---------------------------------------------------------------
  nav_menu(
    title = "Resources", icon = icon('link'),
    align = "right"
    # nav_item(link_khda),
    # nav_item(link_naccho),
    # nav_item(link_lhdmap)
  ),


# debug -------------------------------------------------------------------

  nav_panel(
    title = "Debug", icon = icon('bug'),
    align = "left",
    verbatimTextOutput('sessionInputVariables')
  ),


# mode toggle -------------------------------------------------------------

  
  nav_item(
    input_dark_mode(id = "mode_toggle", mode = "light") 
  ),
) # end page_sidebar
