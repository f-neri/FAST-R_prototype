library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel(
    strong("FAST Data Analysis App")
    ),
  
  sidebarLayout(
    sidebarPanel(
      h1(strong("Input")),
      br(),
      br(),
      h3(em("Inpute files")),
      br(),
      fileInput("Image_Analyst_output_file_name", label = "Image Analyst output file name"),
      fileInput("plate_template_name", label = "Plate template file name"),
      br(),
      h3(em("Additional Parameters")),
      br(),
      numericInput("DAPI_label_number", label = "DAPI label #", value = 1),
      numericInput("EdU_label_number", label = "EdU label #", value = 2),
      numericInput("SABGal_label_number", label = "SA-β-Gal label #", value = 3),
      helpText("Note: the default label # values for DAPI, EdU, and SA-β-Gal should be correct if you followed the
               image analysis protocol correctly"),
      br(),
      numericInput("EdU_threshold_percentile", label = "Percentile EdU treshold", value = 0.95),
      numericInput("SABGal_threshold_percentile", label = "Percentile SA-β-Gal treshold", value = 0.95),
      helpText("Indicate the percentile values to use (0-1) to automatically calculate the positivity thresholds for
               EdU and SA-β-Gal staining (based on background staining)"),
      br(),
      h3(em("Optional Analysis")),
      br(),
      checkboxInput("assess_cell_viability_and_senescence_markers_changes", label = "Assess cell viability and 
                    senescence marker changes based on a treatment? (Optional)"),
      textInput("cell_viability_variable", label = "Treatment variable"),
      helpText("Note: the treatment variable entered must be a perfect match to one of the variable names entered
               in the plate template file"),
      br(),
      h3(em("Output Graphs Settings")),
      br(),
      textInput("RColorBrewer_palette", label = "Color palette (from RColorBrewer)", value = "Dark2"),
      helpText("Find admissible color palette options at https://r-graph-gallery.com/38-rcolorbrewers-palettes.html"),
      checkboxInput("invert_colors", label = "Invert palette colors order?"),
      br(),
      numericInput("resolution_dpi", label = "Graphs resolution (dpi)", value = 600),
      helpText("low: 72; medium: 150; high: 320; ultra high; 600"),
      br(),
      p(strong("Character font for graphs text")),
      numericInput("size_legend_title", label = "Legend title", value = 14),
      numericInput("size_legend_text", label = "Legend text", value = 12),
      numericInput("size_axis_title", label = "Axis title", value = 14),
      numericInput("size_axis_text", label = "Axis text", value = 12),
      numericInput("size_facets_text", label = "Facet text", value = 14)
      ),
    
    
    mainPanel(
      h1("Instructions"),
      br(),
      p("Follow the instructions below to perform run the FAST Data Analysis app:"),
      br()
      )
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)