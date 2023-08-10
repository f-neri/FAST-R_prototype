
# Libraries ---------------------------------------------------------------

library(shiny)
library('shinyDirectoryInput')

library(tidyverse)
library(readxl)
library(plater)
library(stringi)
library(beepr)
library(openxlsx)
library(scales)
library(rgl)
library(htmlwidgets)

# START: UI ---------------------------------------------------------------

ui <- fluidPage(
  
  tags$head( # changes CSS style of validate() text
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
      .shiny-output-error-analysis_completed {
      color: green;
      font-weight: bold;
      }
    "))
  ),
  
  titlePanel(
    strong("FAST Data Analysis App")
    ),
  
  sidebarLayout(
    sidebarPanel(
      h1(strong("Analysis Inputs")),
      br(),
      br(),
      downloadButton("download", label = "Download plate-metadata.csv"),
      br(),
      br(),
      h3(em("Input files")),
      br(),
      fileInput("Image_Analyst_output_file_name", label = "Image Analyst output file", accept = ".xlsx"),
      fileInput("plate_template_name", label = "Plate metadata file",  accept = ".csv"),
      br(),
      h3(em("Staining Labels")),
      br(),
      selectInput("DAPI_label_number", label = "DAPI label #",
                  choices = c(1, 2, 3),
                  selected = 1),
      selectInput("EdU_label_number", label = "EdU label #",
                  choices = c(1, 2, 3),
                  selected = 2),
      selectInput("SABGal_label_number", label = "SA-β-Gal label #",
                  choices = c(1, 2, 3),
                  selected = 3),
      helpText("Note: the default label # values for DAPI, EdU, and SA-β-Gal should be correct if you followed the
               image analysis protocol correctly"),
      br(),
      h3(em("Positive Staining Thresholds")),
      br(),
      numericInput("EdU_threshold_percentile", label = "Percentile EdU threshold", value = 0.95),
      numericInput("SABGal_threshold_percentile", label = "Percentile SA-β-Gal threshold", value = 0.95),
      helpText("Indicate the percentile values to use (0-1) to automatically calculate the positive staining thresholds for
               EdU and SA-β-Gal staining (based on background staining)"),
      br(),
      h3(em("Output Graphs Settings")),
      br(),
      textInput("RColorBrewer_palette", label = "Color palette (from RColorBrewer)", value = "Dark2"),
      helpText("Find admissible color palette options at https://r-graph-gallery.com/38-rcolorbrewers-palettes.html"),
      checkboxInput("invert_colors", label = "Invert palette colors order?"),
      br(),
      numericInput("resolution_dpi", label = "Graphs resolution (dpi)", value = 600), # if changed to selectInput gives issue
      helpText("low: 72; medium: 150; high: 300; ultra high: 600"),
      br(),
      p(strong("Font size for graph text")),
      numericInput("size_legend_title", label = "Legend title", value = 14),
      numericInput("size_legend_text", label = "Legend text", value = 12),
      numericInput("size_axis_title", label = "Axis title", value = 14),
      numericInput("size_axis_text", label = "Axis text", value = 12),
      numericInput("size_facets_text", label = "Facet text", value = 14),
      br(),
      p(strong("Remove text in 3D graphs")),
      checkboxInput("remove_3D_labels", label = "Remove axis labels and plot title from 3D graphs? (Optional)"),
      br(),
      br(),
      h3(em("Optional Analysis")),
      br(),
      checkboxInput("assess_cell_viability_and_senescence_markers_changes", label = "Assess cell viability and 
                    senescence marker changes based on a treatment? (Optional)"),
      textInput("cell_viability_variable", label = "Treatment variable"),
      helpText("Note: the treatment variable entered must be a perfect match to one of the variable names entered
               in the plate metadata file"),
      br(),
      br(),
      h3(em("Save Analysis")),
      directoryInput('directory', label = 'select a folder to save files in'),
      br(),
      h3(em("Run Analysis")),
      br(),
      actionButton("run_button", label = "Run"),
      helpText("Note: the app won't run unless you've selected the 1) Image Analyst output file,
               2) Plate metadata file, and
               3) saving folder")
      ),
    
    
    mainPanel(
      h1(strong("Instructions")),
      br(),
      p("Follow the instructions below to run the FAST Data Analysis app:"),
      br(),
      br(),
      p(strong(em("1) Enter your plate metadata"))),
      br(),
      p("Download the plate-metadata.csv file, and modify it appropriately to match your plate layout."),
      p("○ Indicate which wells contain senescent (e.g. \"SEN\") or non-senescent cells (e.g. \"CTL\") in the Condition plate template."),
      p("○ Indicate which are the background wells in the Condition plate template by adding \"_background\" in the well label (e.g. \"SEN_background\" and \"CTL_background\")."),
      p("○ Optional: add metadata regarding up to 2 additional variables if present in your experiment (e.g. different culturing conditions, or different concentrations of a drug treatment, etc.) in the 2 additional plate templates."),
      p(span(strong("Note")), " If only 1 or no additional variables are present, the empty plate templates can be deleted from the plate-metadata.csv file"),
      br(),
      p(strong(em("2) Select your Image Analyst output and the modified plate-metadata.csv files"))),
      p(span(strong("Note")), " to keep your files more organized, it is recommended to have both of these files in the same folder"),
      br(),
      p(strong(em("3) Modify the analysis parameters to your preferences"))),
      br(),
      p(strong(em("4) Select a folder where you want the analysis results to be saved in"))),
      p(span(strong("Note")), " to keep your files more organized, it is recommended to select the folder that contains both your plate-metadata and Image Analyst output files"),
      br(),
      textOutput("analysis_output")
      )
  )
)


# START: Server -----------------------------------------------------------

server <- function(input, output, session) {
  
  ## download plate-metadata file
  output$download <- downloadHandler(
    filename = function() {
      "plate-metadata.csv"
    },
    
    content = function(file) {
      
      url <- "https://raw.githubusercontent.com/f-neri/FAST-Data-Analysis/main/plate-metadata.csv"
      
      download.file(url, destfile = file, method = "auto")
    }
  )
  
  ## select folder as wd, where analysis results will be saved
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        
        # launch the directory selection dialog with initial path read from the widget
        selected_directory <<- choose.dir(default = readDirectoryInput(session, 'directory'))
        
        # update the widget value
        updateDirectoryInput(session, 'directory', value = selected_directory)
      }
    }
  )
  
  
  analysis_output <- eventReactive(input$run_button, {

# Saving input values to objects ------------------------------------------
    
    ## conditions for execution
    req(input$Image_Analyst_output_file_name, input$plate_template_name, input$directory > 0)
    
    ## progress bar function
    
    if (input$assess_cell_viability_and_senescence_markers_changes == TRUE) {
      tot_steps <- 33
    } else {
      tot_steps <- 24
    }
    
    show_analysis_progress <- function(file_name_generated = "") {
      percentage_progress <<- percentage_progress + 1/tot_steps*100 
      incProgress(1,
                  detail = str_c("Progress: ", round(percentage_progress, digits = 0), "%\n
                                   Generating ",file_name_generated))
    }
    
    ## input files
    Image_Analyst_output_file_name <- input$Image_Analyst_output_file_name$datapath
    plate_template_name <- input$plate_template_name$datapath
    
    ## additional parameters
    DAPI_label_number <- input$DAPI_label_number
    EdU_label_number <- input$EdU_label_number
    SABGal_label_number <- input$SABGal_label_number
    
    EdU_threshold_percentile <- input$EdU_threshold_percentile
    SABGal_threshold_percentile <- input$SABGal_threshold_percentile
    
    ## optional analysis
    assess_cell_viability_and_senescence_markers_changes <- input$assess_cell_viability_and_senescence_markers_changes
    cell_viability_variable <- input$cell_viability_variable
    
    ## output graphs settings
    RColorBrewer_palette <- input$RColorBrewer_palette
    invert_colors <- input$invert_colors ## invert colors order?
    
    resolution_dpi <- input$resolution_dpi
    
    size_legend_title <- input$size_legend_title
    size_legend_text <- input$size_legend_text
    size_axis_title <- input$size_axis_title
    size_axis_text <- input$size_axis_text
    size_facets_text <- input$size_facets_text

    setwd(readDirectoryInput(session, 'directory')) # setting wd based on directory entered as input
    
    percentage_progress <<- 0 # sets progress bar initial value to 0
    
    withProgress(min = 0, max = tot_steps, message = "Analysis is running", value = 0, {
      
      # all script steps must be entered here
      # to be able to see them on progress bar
      

# Tidying IA output: START ------------------------------------------------
      
      show_analysis_progress("tidy Image Analyst output") # updates progress bar
      
      # import IA-output file
      IA_output_unadjusted <- read_xlsx(Image_Analyst_output_file_name, skip = 1, na = "NA")
      
      # adjust new IA output to match older IA output (removing extra columns, changing "Plot Name" to "Name")
      
      cols_to_remove <- c("Folder Name", "Base Name", "Position Name", "Frame")
      
      cols_to_keep <- colnames(IA_output_unadjusted)[!(colnames(IA_output_unadjusted) %in% cols_to_remove)]
      
      IA_output <- IA_output_unadjusted[,cols_to_keep]
      
      if (any(colnames(IA_output) %in% "Plot Name")) {
        colnames(IA_output)[grep("Plot Name", colnames(IA_output))] <- "Name"
      }
      
      # change column names so they're all "OBJ#"
      
      col_names <- colnames(IA_output)
      
      adjusted_col_names <- col_names[-(1:2)] %>%
        grep(pattern = "[.][.][.]", value = TRUE) %>%
        str_sub(start = 4) %>%
        as.numeric() %>%
        -2 %>%
        str_c("OBJ", .)
      
      final_col_names <- col_names %>%
        grep(pattern = "[.][.][.]", invert = TRUE, value = TRUE) %>%
        c(., adjusted_col_names)
      
      colnames(IA_output) <- final_col_names
      
      # tidy the dataset
      
      ## pivot_longer()
      
      ### creating character vector with all OBJ# present in the data frame header
      
      OBJ_vec <- colnames(IA_output)[-(1:2)]
      
      ### pivoting
      tidy_data1 <- IA_output %>%
        pivot_longer(all_of(OBJ_vec),
                     names_to = "cell_ID",
                     values_to = "Signal_Intensity")
      
      ## splitting "Name" Column into well # and the parameter measured
      tidy_data2 <- mutate(tidy_data1,
                           well = sub(".+ - ([0-9]+ )*", "", tidy_data1$Name),
                           Measured_Parameter = sub(" - .*", "", tidy_data1$Name)
      )
      
      tidy_data2$well <- ifelse(nchar(tidy_data2$well) == 2, sub(pattern = "(.)(.)", replacement = "\\10\\2", tidy_data2$well), tidy_data2$well)
      
      ## eliminating "Name" and "Channel" columns, rearranging tibble
      tidy_data3 <- select(tidy_data2, -(Name:Channel)) %>%
        select(well, cell_ID,everything())
      
      ## pivot_wider()
      tidy_data4 <- tidy_data3 %>%
        pivot_wider(names_from = Measured_Parameter,
                    values_from = Signal_Intensity)
      
      ## removing non-existing cells from each well (i.e. where signal intensities = NA)
      tidy_data5 <- tidy_data4 %>% na.omit()
      
      # importing metadata regarding conditions of each well (IR or CTL, full serum or serum-starved, different drug concentrations etc.)
      
      ## plater
      file_path <- plate_template_name # gets string with full path
      
      plate_metadata <- read_plate(
        file = file_path,             # full path to the .csv file
        well_ids_column = "well",    # name to give column of well IDs (optional)
        sep = ","                     # separator used in the csv file (optional)
      )
      
      ## change variable names to lower case, removing parenthesis (if present)
      colnames(plate_metadata) <- colnames(plate_metadata) %>%
        tolower() %>%
        str_replace_all(., "[()]", "")
      
      ## check that plate-template contains Condition variable
      if (any(colnames(plate_metadata) %in% "condition") == TRUE) {} else {
        beep(1)
        validate(
          "ERROR: The metadata entered in plate-template must contain the \"Condition\" variable"
        )}
      
      is_alphanumeric <- function(input_string) {
        # The regular expression checks for alphanumeric characters.
        # "^" at the start and "$" at the end ensure the whole string must match, not just part.
        if (grepl("^[a-zA-Z0-9 _]+$", input_string)) {TRUE} else {FALSE}
      }
      
      variables_alphanumeric <- vector("character", 3)
      
      for (i in seq_along(colnames(plate_metadata))) {
        variables_alphanumeric[[i]] <- is_alphanumeric(colnames(plate_metadata)[[i]])
      }
      
      if (any(variables_alphanumeric == FALSE)) {
        beep(1)
        validate(
          "ERROR: The metadata entered in plate-template is not acceptable.

Variable names must be alphanumeric (only a-z letters and 0-9 digits are allowed)")
      }
      
      ## check that the variables contained in plate-template are limited to
      ## condition, and up to 2 additional variable (e.g. serum and/or drug treatment)
      additional_variables <- colnames(plate_metadata) %>%
        .[-grep(pattern = "well|condition", .)]
      
      if (length(additional_variables) > 2) {
        beep(1)
        validate(
          "ERROR: The metadata entered in plate-template is not acceptable.
          
          More than 3 variables have been entered.

The only variables that can be entered in the plate-template file are
\"Condition\" and up to TWO more variable (e.g. \"Serum\" and/or \"Drug_concentration\")")
      }
      
      additional_variables_check <-  if (length(additional_variables) > 0) {TRUE} else {FALSE}
      multiple_additional_variables_check <- if (length(additional_variables) == 2) {TRUE} else {FALSE}
      
      # add metadata info (conditions & serum) to table with observations (signal intensities for each cell)
      
      ## mutating join with left_join()
      
      plate_metadata_variables <- colnames(plate_metadata)[-1]
      
      tidy_data6 <- tidy_data5 %>%
        left_join(plate_metadata, by = "well") %>%
        select(well, cell_ID, all_of(plate_metadata_variables), everything())
      
      # changing the column names based on the strings entered at the beginning of script (READ ME section)
      
      ## changing current variable/column names to the ones inputted in the READ ME section
      colnames(tidy_data6)[str_detect(colnames(tidy_data6), "Plot of Each")] <- "Nuclear_Area"
      
      latest_value <- colnames(tidy_data6)
      
      param <- tibble(previous = list(DAPI_label_number, EdU_label_number, SABGal_label_number),
                      new = list("DAPI","EdU","SABGal"))
      
      update_value <- function(previous, new) {
        latest_value <<- sub(pattern = str_c(".+Label #", previous, ".*"), replacement = new, latest_value)
      } # function needs "<<-" in order to update the global variable "latest_value"
      
      map2(param$previous, param$new, update_value)
      
      colnames(tidy_data6) <- latest_value
      
      "completed tidying IA output"
      

# Plotting SABGal and EdU graphs: START -----------------------------------
      
      show_analysis_progress("graphs settings")
      
      ## input df
      df <- tidy_data6
      
      ## checking for additional variables; if present, renaming corresponding variables entered in plate-template
      if (additional_variables_check == TRUE) {
        
        additional_variable_1 <- additional_variables[1]
        new_col_names <- colnames(df) %>%
          str_replace(., additional_variable_1, "additional_variable_1")
        
        new_col_names <- str_replace_all(colnames(df), regex(paste0("\\b", additional_variable_1, "\\b")), "additional_variable_1")
        
        str_replace_all(colnames(df), regex(paste0("\\b", gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", additional_variable_1)), "\\b")), "additional_variable_1")
        
        if (multiple_additional_variables_check == TRUE) {
          additional_variable_2 <- additional_variables[2]
          new_col_names <- new_col_names %>%
            str_replace(., additional_variable_2, "additional_variable_2")
        }
        
        colnames(df) <- new_col_names # renames additional variables present
        
        if (multiple_additional_variables_check == TRUE) {
          if (length(unique(df$additional_variable_1)) < length(unique(df$additional_variable_2))) {
            new_col_names[4:5] <- new_col_names[5:4]
            colnames(df) <- new_col_names
            temp <- additional_variable_1
            additional_variable_1 <- additional_variable_2
            additional_variable_2 <- temp
          } # assigns additional_variable_1 to the additional variable with the highest # of possible values, so that such variable is used for facet_grid cols later
        }
      }
      
      # graphs settings (folder, themes, dimensions, etc) ----------------------
      
      ## setting grouping variables to use in signal_by_well plots
      
      grouping_arguments <- c("well", "condition")
      if(additional_variables_check == TRUE) {
        grouping_arguments <- append(grouping_arguments, "additional_variable_1")
        if(multiple_additional_variables_check) {grouping_arguments <- append(grouping_arguments, "additional_variable_2")}
      }
      
      ## creating graph folder
      
      graphs_folder <- str_c("Analysis_", input$Image_Analyst_output_file_name$name,"_", Sys.time()) %>%
        str_replace_all(pattern = " ", replacement = "_") %>%
        str_replace_all(pattern = ":", replacement = "") %>%
        str_replace_all(pattern = ".xlsx", replacement = "")
      
      dir.create(graphs_folder)
      
      ## ggplot themes
      
      old_theme <- theme_set(theme_bw())
      theme_update(
        legend.title = element_text(size=size_legend_title), #change legend title font size
        legend.text = element_text(size=size_legend_text), #change legend text font size
        axis.title = element_text(size=size_axis_title),
        axis.text = element_text(size=size_axis_text),
        strip.text = element_text(size=size_facets_text)
      )
      
      # graph limits
      
      lower_limit_SABGal <- quantile(df$SABGal,0.02)
      upper_limit_SABGal <- quantile(df$SABGal,0.98)
      
      lower_limit_EdU <- quantile(df$EdU,0.02)
      upper_limit_EdU <- quantile(df$EdU,0.98)
      
      # conditions colors
      
      color_scale_conditions <- scale_color_brewer(palette = RColorBrewer_palette, limits = sort(unique(df$condition), decreasing = invert_colors))
      fill_scale_conditions <- scale_fill_brewer(palette = RColorBrewer_palette, limits = sort(unique(df$condition), decreasing = invert_colors))
      
      ## generating evenly-spaced axis ticks for log-scaled axis
      x_axis_ticks <- seq(from = 0, to = round(log10(upper_limit_SABGal))) %>%
        10^.
      
      y_axis_ticks <- seq(from = 0, to = round(log10(upper_limit_EdU))) %>%
        10^.
      
      ## creating default png dimensions
      
      grid_col_n <- if (additional_variables_check == TRUE) {
        plate_metadata %>%
          select(all_of(additional_variable_1)) %>%
          unique() %>%
          nrow()
      } else {1}
      
      default_width <- 300 * (resolution_dpi/72) + 180 * grid_col_n * (resolution_dpi/72)
      
      grid_row_n <- if (multiple_additional_variables_check == TRUE) {
        plate_metadata %>%
          select(all_of(additional_variable_2)) %>%
          unique() %>%
          nrow()
      } else {1}
      
      default_height <- 50 * (resolution_dpi/72) + 200 * grid_row_n * (resolution_dpi/72)
      
      default_png <- function(filename = file_path, width = default_width, height = default_height) {
        ggsave(filename = filename, device = "png", width = width, height = height, units = "px", dpi = resolution_dpi)
      }
      
      conditions_n <- df$condition %>%
        unique() %>%
        length()
      
      conditions_n_signal <- conditions_n[!( str_detect( conditions_n , "_background" ) )] %>% length()
      
      
      ## graph counter
      
      graph_counter <- 1
      
      # generating SABGal & EdU signal thresholds --------------------------------
      
      ## finds condition values containing "background"
      background_conditions <- unique(df$condition)[str_detect(unique(df$condition), "background")]
      
      ## creates df containing only background conditions (i.e. conditions that contain "background" in their name)
      filter_only_background <- function(x) {
        filter(x, condition %in% background_conditions)
      }
      
      filter_not_background <- function(x) {
        filter(x, !(condition %in% background_conditions))
      }
      
      df_background <- filter_only_background(df)
      
      ## overall thresholds (all background cells)
      EdU_threshold_overall <- quantile(df_background$EdU, EdU_threshold_percentile)
      SABGal_threshold_overall <- quantile(df_background$SABGal, SABGal_threshold_percentile)
      
      ## thresholds for all condition + groups (additional variables) combinations
      
      ### thresholds that distinguish conditions (e.g. CTL vs SEN); used for % calculations
      grouping_arguments_no_well <- grouping_arguments %>%
        str_remove("well") %>%
        .[nzchar(.)]
      
      df_background_thresholds <- df_background %>%
        group_by(!!!syms(grouping_arguments_no_well)) %>%
        summarise(EdU_threshold = quantile(EdU, EdU_threshold_percentile),
                  SABGal_threshold = quantile(SABGal, SABGal_threshold_percentile),
                  group_cell_count = n())
      
      ### using overall thresholds for groups with <100 cells
      if (any(df_background_thresholds$group_cell_count < 100)) {
        df_background_thresholds$EdU_threshold_overall <- EdU_threshold_overall
        df_background_thresholds$SABGal_threshold_overall <- SABGal_threshold_overall
        df_background_thresholds$EdU_threshold <- ifelse(df_background_thresholds$group_cell_count < 100, df_background_thresholds$EdU_threshold_overall, df_background_thresholds$EdU_threshold)
        df_background_thresholds$SABGal_threshold <- ifelse(df_background_thresholds$group_cell_count < 100, df_background_thresholds$SABGal_threshold_overall, df_background_thresholds$SABGal_threshold)
      }
      
      df_background_thresholds$group_cell_count <- NULL
      
      ### using overall thresholds for groups without background wells
      grouping_arguments_no_well_no_condition <- grouping_arguments_no_well %>%
        str_remove("condition") %>%
        .[nzchar(.)]
      
      if (length(grouping_arguments_no_well_no_condition) >= 1) {
        
        df_background_thresholds_groups <- df_background_thresholds %>%
          group_by(!!!syms(grouping_arguments_no_well_no_condition)) %>%
          summarise()
        
        df_groups <- df %>%
          group_by(!!!syms(grouping_arguments_no_well_no_condition)) %>%
          summarise()
        
        groups_to_add <- anti_join(df_groups, df_background_thresholds_groups)
        
        df_background_thresholds_conditions <- df_background_thresholds %>%
          group_by(condition) %>%
          summarise()
        
        groups_to_add <- crossing(df_background_thresholds_conditions, groups_to_add)
        groups_to_add$EdU_threshold <- EdU_threshold_overall
        groups_to_add$SABGal_threshold <- SABGal_threshold_overall
        
      }
      
      ### average thresholds of different conditions (e.g. CTL vs SEN); used to plot thresholds in signal graphs
      
      if (length(grouping_arguments_no_well_no_condition) >= 1) {
        df_background_thresholds_average <- df_background_thresholds %>%
          group_by(!!!syms(grouping_arguments_no_well_no_condition)) %>%
          summarise(EdU_threshold_average = mean(EdU_threshold),
                    SABGal_threshold_average = mean(SABGal_threshold))
      } else {
        df_background_thresholds_average <- df_background_thresholds %>%
          mutate(EdU_threshold_average = mean(df_background_thresholds$EdU_threshold),
                 SABGal_threshold_average = mean(df_background_thresholds$SABGal_threshold))
      }
      
      df_background_thresholds <- df_background_thresholds %>%
        left_join(df_background_thresholds_average)
      
      df_signal_thresholds <- df_background_thresholds
      
      df_signal_thresholds$condition <- str_replace_all(df_signal_thresholds$condition, "_background", "")
      
      df_all_thresholds <- rbind(df_background_thresholds, df_signal_thresholds)
      
      ## df with thresholds
      df_with_thresholds <- df %>%
        left_join(df_all_thresholds)
      
      df_with_thresholds$EdU_threshold_overall <- EdU_threshold_overall
      df_with_thresholds$SABGal_threshold_overall <- SABGal_threshold_overall
      
      ## substituting NA thresholds (if present) with overall SABGal and EdU thresholds
      if (any(is.na(df_with_thresholds$EdU_threshold))) {
        
        df_with_thresholds_only_NA <- df_with_thresholds[!(complete.cases(df_with_thresholds)),]
        
        df_with_thresholds_only_NA$EdU_threshold <- df_with_thresholds_only_NA$EdU_threshold_overall
        df_with_thresholds_only_NA$EdU_threshold_average <- df_with_thresholds_only_NA$EdU_threshold_overall
        
        df_with_thresholds_only_NA$SABGal_threshold <- df_with_thresholds_only_NA$SABGal_threshold_overall
        df_with_thresholds_only_NA$SABGal_threshold_average <- df_with_thresholds_only_NA$SABGal_threshold_overall
        
        df_with_thresholds_no_NA <- df_with_thresholds[complete.cases(df_with_thresholds),]
        
        df_with_thresholds <- rbind(df_with_thresholds_no_NA, df_with_thresholds_only_NA)
      }
      
      ## grouped df with thresholds for by_well graphs
      
      by_well <- df_with_thresholds %>%
        group_by(!!!syms(grouping_arguments))
      
      # graph signal each cell (w/ background) ----------------------------------
      
      show_analysis_progress("graph: signal_each-cell.png")
      
      ## ggplot function
      
      plot_signal_each_cell <- function(x) {
        
        ggplot(data = x, aes(SABGal, EdU)) +
          geom_point(
            aes(color = condition),
            alpha = 1/8
          ) +
          {if (length(additional_variables) == 1)
            facet_grid(cols = vars(additional_variable_1)) } +
          {if (length(additional_variables) == 2)
            facet_grid(cols = vars(additional_variable_1),
                       rows = vars(additional_variable_2)) } +
          geom_vline(data = df_all_thresholds, aes(xintercept = SABGal_threshold_average)) +
          geom_hline(data = df_all_thresholds, aes(yintercept = EdU_threshold_average)) +
          scale_x_continuous(name = expression("Integrated SA-\u03B2-Gal OD"),
                             trans = pseudo_log_trans(base = 10),
                             limits = c(lower_limit_SABGal, upper_limit_SABGal), # added axis limits to zoom in where relevant
                             breaks = x_axis_ticks
          ) +
          scale_y_continuous(name = "Integrated EdU intensity (AU)",
                             trans = pseudo_log_trans(base = 10),
                             limits = c(lower_limit_EdU, upper_limit_EdU), # added axis limits to zoom in where relevant
                             breaks = y_axis_ticks
          ) +
          labs(color = "Condition") +
          color_scale_conditions +
          guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                          size = 3))) # to make enlarge points in legend
      }
      
      ## plotting scatterplot SA-B-Gal and EdU signal & background with each individual cell
      
      file_path <- str_c(getwd(),"/",graphs_folder, "/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_signal_each-cell.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_signal_each_cell(df)
      
      default_png()# saves the plot above to PNG
      
      # graph signal each cell (w/o background) -----------------------
      
      show_analysis_progress("graph: signal_each-cell_no-backgr.png")
      
      ## plotting scatterplot SA-B-Gal and EdU signal with each individual cell
      
      df_signal <- filter_not_background(df)
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_signal_each-cell_no-backgr.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_signal_each_cell(df_signal)
      
      default_png()# saves the plot above to PNG
      
      # graph signal each cell (only background) -----------------------
      
      show_analysis_progress("graph: signal_each-cell_only-backgr.png")
      
      df_only_background <- filter_only_background(df)
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_signal_each-cell_only-backgr.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_signal_each_cell(df_only_background)
      
      default_png()# saves the plot above to PNG
      
      # graph percentages all cells (w/ background)---------------------------------------------------
      
      show_analysis_progress("graph: percentage_all-cells.png")
      
      ## adding variables to distinguish cells above and below thresholds
      df_positivity <- mutate(df_with_thresholds,
                              EdU_pos = if_else(EdU >= EdU_threshold, "EdU +", "EdU -"),
                              SABGal_pos = if_else(SABGal >= SABGal_threshold,"SAβGal +", "SAβGal -")) %>%
        unite(c(EdU_pos,SABGal_pos), col = "Positivity", sep = "  ", remove = F)
      
      ## creating tibble with counts of cells above and below thresholds
      grouping_arguments_positivity_x <- c(grouping_arguments_no_well, "Positivity")
      
      x <- df_positivity %>%
        group_by(!!!syms(grouping_arguments_positivity_x)) %>%
        count()
      
      y <- df_positivity %>%
        group_by(!!!syms(grouping_arguments_no_well)) %>%
        count()
      
      colnames(y)[colnames(y) == "n"] <- "Sum"
      
      ## any of condition and additional variables included in x or y
      df_percent <- left_join(x,y, by = grouping_arguments_no_well) %>%
        mutate(Percent = n/Sum)
      
      ## plotting function
      plot_percentage_all_cells <- function(data) {
        ggplot(data, aes(x = Percent, y = condition, fill = Positivity)) +
          geom_col(position = "fill") +
          labs(y = "Condition") +
          geom_text(aes(label = ifelse(Percent <= 0.10, "", percent(Percent, accuracy = 1.0))),
                    position = position_fill(vjust = 0.5),
                    size = size_axis_text/3) +
          scale_x_continuous(labels = percent, name = NULL, breaks = NULL) +
          {if (length(additional_variables) == 1)
            facet_grid(cols = vars(additional_variable_1)) } +
          {if (length(additional_variables) == 2)
            facet_grid(cols = vars(additional_variable_1),
                       rows = vars(additional_variable_2)) } +
          scale_fill_manual(values = c("gray88", "gray50", "green", "darkgreen")) +
          guides(fill = guide_legend(title = NULL)) +
          theme(legend.key.height = unit(0.5, 'cm'), #change legend key height
                legend.key.width = unit(0.5, 'cm')) #change legend key width) #change legend key size
      }
      
      ## generating bar graph as png file
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_percentage_all-cells.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_percentage_all_cells(df_percent)
      
      default_png(height = 60 * (resolution_dpi/72) + 30*conditions_n * grid_row_n * (resolution_dpi/72),
                  width = default_width + 60 * (resolution_dpi/72)) # saves the plot above to PNG
      
      
      
      # graph percentages all cells (w/o background) ----------------------------------
      
      show_analysis_progress("graph: percentage_all-cells_no-backgr.png")
      
      ## filtering to remove background wells
      df_percent_no_background <- filter_not_background(df_percent)
      
      ## generating bar graph as png file
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_percentage_all-cells_no-backgr.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_percentage_all_cells(df_percent_no_background)
      
      default_png(height = 60 * (resolution_dpi/72) + 30*conditions_n_signal * grid_row_n * (resolution_dpi/72)) # saves the plot above to PNG
      
      
      
      # graph percentages all cells (only background) ---------------------------
      
      show_analysis_progress("graph: percentage_all-cells_only-backgr.png")
      
      ## filtering to remove non-background wells
      df_percent_only_background <- filter_only_background(df_percent)
      
      ## generating bar graph as png file
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_percentage_all-cells_only-backgr.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_percentage_all_cells(df_percent_only_background)
      
      default_png(height = 60 * (resolution_dpi/72) + 30*conditions_n_signal * grid_row_n * (resolution_dpi/72),
                  width = default_width + 60 * (resolution_dpi/72)) # saves the plot above to PNG
      
      
      # graph area each cell ----------------------------------------------------
      
      show_analysis_progress("graph: area_each-cell.png")
      
      ## df_Nuclear Area
      
      df_area <- df
      
      df_area$condition <- df_area$condition %>%
        str_remove_all(., "_background") %>%
        as.factor()
      
      if (additional_variables_check == TRUE) {
        df_area$additional_variable_1 <- df_area$additional_variable_1 %>% as.factor() ## this might be unnecessary
      }
      
      ## violin plot nuclear area
      
      ggplot(df_area, aes(x = condition, y = Nuclear_Area)) +
        geom_violin(aes(fill = condition),
                    trim = FALSE) +
        geom_boxplot(width = 0.05,
                     outlier.shape = NA) +
        labs(y = expression(paste("Nuclear Area (px"^"2", ")")),
             x = "Condition") +
        {if (length(additional_variables) == 1)
          facet_grid(cols = vars(additional_variable_1)) } +
        {if (length(additional_variables) == 2)
          facet_grid(cols = vars(additional_variable_1),
                     rows = vars(additional_variable_2)) } +
        fill_scale_conditions
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_area_each-cell.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      default_png()# saves the plot above to PNG
      
      # graph signal by well (w/ background)------------------------------------
      
      show_analysis_progress("graph: signal_by-well.png")
      
      ## creating summary table and computing median, 25th, 75th percentile
      
      df_summary_signal <- summarise(by_well,
                                     SABGal_Median = median(SABGal),
                                     SABGal_quartile3 = quantile(SABGal, 0.75),
                                     SABGal_quartile1 = quantile(SABGal, 0.25),
                                     SABGal_IQR = SABGal_quartile3 - SABGal_quartile1,
                                     SABGal_upper_outlier_limit = SABGal_quartile3 + 1.5*SABGal_IQR,
                                     SABGal_max = max(SABGal[SABGal < SABGal_upper_outlier_limit]),
                                     SABGal_lower_outlier_limit = SABGal_quartile1 - 1.5*SABGal_IQR,
                                     SABGal_min = min(SABGal[SABGal > SABGal_lower_outlier_limit]),
                                     
                                     EdU_Median = median(EdU),
                                     EdU_quartile3 = quantile(EdU, 0.75),
                                     EdU_quartile1 = quantile(EdU, 0.25),
                                     EdU_IQR = EdU_quartile3 - EdU_quartile1,
                                     EdU_upper_outlier_limit = EdU_quartile3 + 1.5*EdU_IQR,
                                     EdU_max = max(EdU[EdU < EdU_upper_outlier_limit]),
                                     EdU_lower_outlier_limit = EdU_quartile1 - 1.5*EdU_IQR,
                                     EdU_min = min(EdU[EdU > EdU_lower_outlier_limit]),
                                     
                                     Area_median = median(Nuclear_Area)
      )
      
      df_summary_signal <- mutate(df_summary_signal,
                                  SABGal_plot_max = ifelse(SABGal_max > upper_limit_SABGal, upper_limit_SABGal, SABGal_max),
                                  SABGal_plot_min = ifelse(SABGal_min < lower_limit_SABGal, lower_limit_SABGal, SABGal_min),
                                  EdU_plot_max = ifelse(EdU_max > upper_limit_EdU, upper_limit_EdU, EdU_max),
                                  EdU_plot_min = ifelse(EdU_min < lower_limit_EdU, lower_limit_EdU, EdU_min)
      )
      
      plot_signal_grouped_by_well <- function(data) {
        
        ggplot(data = data,
               aes(x = SABGal_Median,
                   y = EdU_Median,
                   ymax = EdU_quartile3,
                   ymin = EdU_quartile1,
                   xmax = SABGal_quartile3,
                   xmin = SABGal_quartile1,
                   color = condition)
        ) +
          geom_errorbar(alpha = 1/3,
                        aes(ymax = EdU_plot_max,
                            ymin = EdU_plot_min),
                        linetype = "dashed") +
          geom_errorbar() +
          geom_errorbarh(alpha = 1/3,
                         aes(xmax = SABGal_plot_max,
                             xmin = SABGal_plot_min),
                         linetype = "dashed") +
          geom_errorbarh() +
          geom_point(size = 3,
                     shape = 21,
                     aes(fill = condition),
                     color = "black") +
          {if (length(additional_variables) == 1)
            facet_grid(cols = vars(additional_variable_1)) } +
          {if (length(additional_variables) == 2)
            facet_grid(cols = vars(additional_variable_1),
                       rows = vars(additional_variable_2)) } +
          geom_vline(data = df_all_thresholds, aes(xintercept = SABGal_threshold_average)) +
          geom_hline(data = df_all_thresholds, aes(yintercept = EdU_threshold_average)) +
          scale_x_continuous(name = expression("Integrated SA-\u03B2-Gal OD"),
                             trans = pseudo_log_trans(base = 10),
                             limits = c(lower_limit_SABGal, upper_limit_SABGal),
                             breaks = x_axis_ticks
          ) +
          scale_y_continuous(name = "Integrated EdU intensity (AU)",
                             trans = pseudo_log_trans(base = 10),
                             limits = c(lower_limit_EdU, upper_limit_EdU),
                             breaks = y_axis_ticks
          ) +
          labs(color = "Condition",
               fill = "Condition") +
          color_scale_conditions +
          fill_scale_conditions +
          guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                          size = 3)))
        
      }
      
      ## plotting scatterplot SA-B-Gal & EdU grouped by well (median, 25th-75th percentile)
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_signal_by-well.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_signal_grouped_by_well(df_summary_signal)
      
      default_png()# saves the plot above to PNG
      
      
      
      # graph signal by well (w/o background) -------------------
      
      show_analysis_progress("graph: signal_by-well_no-backgr.png")
      
      ## filtering background cells/wells out
      
      df_summary_signal_no_background <- filter_not_background(df_summary_signal)
      
      ## plotting scatterplot SA-B-Gal & EdU grouped by well (median, 25th-75th percentile)
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_signal_by-well_no-backgr.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_signal_grouped_by_well(df_summary_signal_no_background)
      
      default_png()# saves the plot above to PNG
      
      
      
      # graph signal by well (only background) -------------------
      
      show_analysis_progress("graph: signal_by-well_only-backgr.png")
      
      df_summary_signal_only_background <- filter_only_background(df_summary_signal)
      
      ## plotting scatterplot SA-B-Gal & EdU grouped by well (median, 25th-75th percentile)
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_signal_by-well_only-backgr.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_signal_grouped_by_well(df_summary_signal_only_background)
      
      default_png()# saves the plot above to PNG
      
      # graph percentages grouped by well  (w/ background)-----------------------------
      
      show_analysis_progress("graph: percentage_by-well.png")
      
      ## creating summary grouped df
      df_summary_percentage <- summarise(by_well,
                                         Tot_Count = n(),
                                         SABGal_Positive = sum(SABGal >= SABGal_threshold),
                                         EdU_Positive = sum(EdU >= EdU_threshold),
                                         SABGal_Positive_Percentage = SABGal_Positive / Tot_Count,
                                         EdU_Positive_Percentage = EdU_Positive / Tot_Count,
                                         Area_median = median(Nuclear_Area))
      
      ## ggplot
      plot_percentages_grouped_by_well <- function(data) {
        ggplot(data, aes(SABGal_Positive_Percentage, EdU_Positive_Percentage)) +
          geom_point(
            shape = 21,
            color = "black",
            aes(fill = condition),
            size = 3) +
          {if (length(additional_variables) == 1)
            facet_grid(cols = vars(additional_variable_1)) } +
          {if (length(additional_variables) == 2)
            facet_grid(cols = vars(additional_variable_1),
                       rows = vars(additional_variable_2)) } +
          coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
          scale_x_continuous(labels = percent, name = "% SA-\u03B2-Gal+ cells",
                             breaks = seq(0, 1, by = 0.2)) +
          scale_y_continuous(labels = percent, name = "% EdU+ cells",
                             breaks = seq(0, 1, by = 0.2)) +
          labs(fill = "Condition") +
          fill_scale_conditions +
          guides(color = guide_legend(override.aes = list(size = 3)))
      }
      
      ## plotting scatterplot
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_percentage_by-well.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_percentages_grouped_by_well(df_summary_percentage)
      
      default_png()# saves the plot above to PNG
      
      
      
      # graph percentages grouped by well  (w/o background) -------------
      
      show_analysis_progress("graph: percentage_by-well_no-backgr.png")
      
      ## creating scatterplot for SA-B-Gal and EdU positivity, where cells are grouped by well
      ## without background wells
      
      ## filtering out background wells from summary table
      
      df_summary_percentage_no_background <- filter_not_background(df_summary_percentage)
      
      ## plotting scatterplot
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_percentage_by-well_no-backgr.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_percentages_grouped_by_well(df_summary_percentage_no_background)
      
      default_png()# saves the plot above to PNG
      
      # graph percentages grouped by well  (only background) -------------
      
      show_analysis_progress("graph: percentage_by-well_only-backgr.png")
      
      df_summary_percentage_only_background <- filter_only_background(df_summary_percentage)
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_percentage_by-well_only-backgr.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_percentages_grouped_by_well(df_summary_percentage_only_background)
      
      default_png()# saves the plot above to PNG
      
      # graph Area by well ------------------------------------------------------
      
      show_analysis_progress("graph: median_area_by-well.png")
      
      plot_median_signal_boxplot <- function(data, variable, variable_label) {
        
        ggplot(data,
               aes(y = condition,
                   x = variable,
                   color = condition)) +
          geom_boxplot(linewidth = 0.25) +
          geom_point(size = 3,
                     shape = 21,
                     color = "black",
                     aes(fill = condition)) +
          labs(
            color = "Condition",
            fill = "Condition",
            y = "Condition",
            x = variable_label
          ) +
          {if (length(additional_variables) == 1)
            facet_grid(cols = vars(additional_variable_1)) } +
          {if (length(additional_variables) == 2)
            facet_grid(cols = vars(additional_variable_1),
                       rows = vars(additional_variable_2)) } +
          guides(colour = guide_legend(override.aes = list(size = 3))) +
          color_scale_conditions +
          fill_scale_conditions
        
      }
      
      plot_median_signal_boxplot(df_summary_signal_no_background, df_summary_signal_no_background$Area_median, expression(paste("Median Nuclear Area (px"^"2", ")")))
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_median_area_by-well.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      default_png(height =  10 * (size_axis_title/14) * (resolution_dpi/72) + 90 * (resolution_dpi/72) + 40*conditions_n_signal * grid_row_n * (resolution_dpi/72),
                  width = default_width - 120 * (resolution_dpi/72)) # saves the plot above to PNG
      
      
      # graph EdU by well ------------------------------------------------------
      
      show_analysis_progress("graph: median_EdU_by-well.png")
      
      plot_median_signal_boxplot(df_summary_signal_no_background, df_summary_signal_no_background$EdU_Median, "Median EdU intensity (AU)")
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_median_EdU_by-well.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      default_png(height = 90 * (resolution_dpi/72) + 40*conditions_n_signal * grid_row_n * (resolution_dpi/72),
                  width = default_width - 120 * (resolution_dpi/72)) # saves the plot above to PNG
      
      
      # graph SABGal by well ------------------------------------------------------
      
      show_analysis_progress("graph: median_SABGal_by-well.png")
      
      plot_median_signal_boxplot(df_summary_signal_no_background, df_summary_signal_no_background$SABGal_Median, "Median SA-\u03B2-Gal OD")
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_median_SABGal_by-well.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      default_png(height = 90 * (resolution_dpi/72) + 40*conditions_n_signal * grid_row_n * (resolution_dpi/72),
                  width = default_width - 120 * (resolution_dpi/72)) # saves the plot above to PNG
      
      

      # graph Area by well - fold change --------------------------------
      
      ## create df with averages for median signals
      
      df_median_signal_average <- df_summary_signal_no_background %>%
        group_by(!!!syms(grouping_arguments_no_well)) %>%
        summarise(
          average_SABGal_Median = mean(SABGal_Median),
          average_EdU_Median = mean(EdU_Median),
          average_Area_median = mean(Area_median)
          )
      
      ## generate min values that will be set as reference for fold changes (value = 1)
      
      average_SABGal_Median_min <- min(df_median_signal_average$average_SABGal_Median)
      average_EdU_Median_min <-  min(df_median_signal_average$average_EdU_Median)
      average_Area_Median_min <- min(df_median_signal_average$average_Area_median)
      
      ## add fold change values to df_summary_signal_no_background
      
      df_summary_signal_no_background <- df_summary_signal_no_background %>%
        mutate(
          fold_change_SABGal = SABGal_Median/average_SABGal_Median_min,
          fold_change_EdU = EdU_Median/average_EdU_Median_min,
          fold_change_Area = Area_median/average_Area_Median_min
        )
      
      
      ## plot Area
      
      show_analysis_progress("graph: median_Area_by-well_fold-change.png")
      
      plot_median_signal_boxplot(df_summary_signal_no_background, df_summary_signal_no_background$fold_change_Area, "fold change Nuclear Area")
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_median_Area_by-well_fold-change.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      default_png(height = 90 * (resolution_dpi/72) + 40*conditions_n_signal * grid_row_n * (resolution_dpi/72),
                  width = default_width - 120 * (resolution_dpi/72)) # saves the plot above to PNG
      
      
      # graph EdU by well - fold change --------------------------------
      
      ## plot EdU
      
      show_analysis_progress("graph: median_EdU_by-well_fold-change.png")
      
      plot_median_signal_boxplot(df_summary_signal_no_background, df_summary_signal_no_background$fold_change_EdU, "fold change EdU")
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_median_EdU_by-well_fold-change.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      default_png(height = 90 * (resolution_dpi/72) + 40*conditions_n_signal * grid_row_n * (resolution_dpi/72),
                  width = default_width - 120 * (resolution_dpi/72)) # saves the plot above to PNG
      
      # graph SABGal by well - fold change --------------------------------
      
      ## plot SABGal
      
      show_analysis_progress("graph: median_SABGal_by-well_fold-change.png")
      
      plot_median_signal_boxplot(df_summary_signal_no_background, df_summary_signal_no_background$fold_change_SABGal, "fold change SA-\u03B2-Gal")
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_median_SABGal_by-well_fold-change.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      default_png(height = 90 * (resolution_dpi/72) + 40*conditions_n_signal * grid_row_n * (resolution_dpi/72),
                  width = default_width - 120 * (resolution_dpi/72)) # saves the plot above to PNG
      
      
      
      
      # graph 3D: median area, SABGal, EdU -------------------------------------
      
      show_analysis_progress("graph: median-signal_SABGal-EdU-NuclearArea.html")
      
      ## adding color column
      
      plot3D_colors <- tibble("colors" = RColorBrewer::brewer.pal(length(unique(df_summary_signal$condition)), RColorBrewer_palette),
                              "condition" = sort(unique(df_summary_signal$condition)))
      
      df_summary_signal_no_background <- left_join(df_summary_signal_no_background, plot3D_colors)
      
      ## adding graph limits (for consistency across 3D plots)
      
      generate_limits <- function(variable) {
        c(min(df_summary_signal_no_background[[variable]]), max(df_summary_signal_no_background[[variable]]))
      }
      
      variables <- c("SABGal_Median", "EdU_Median", "Area_median")
      
      limits_list <- vector("list", length = 3)
      
      for (i in seq_along(variables)) {
        limits_list[[i]] <- generate_limits(variables[[i]])
      }
      
      adjust_limits <- function(limits_number) {
        extra_room <<- (limits_list[[limits_number]][2] - limits_list[[limits_number]][1])*0.03
        
        limits_list[[limits_number]] <<- c(limits_list[[limits_number]][1] - extra_room, limits_list[[limits_number]][2] + extra_room)
        
        limits_list[[limits_number]] <<- limits_list[[limits_number]] %>% round()
      }
      
      for (i in seq_along(limits_list)) {
        adjust_limits(i)
      }
      
      ## 3D plot function
      
      plot_3D_median_signal <- function(data, graph_name) { ## working
        open3d() ## this is needed to open up a new rgl widget with each function run; otherwise, new runs will add points to previous plots
        plot3d(x = data$SABGal_Median,
               y = data$EdU_Median,
               z = data$Area_median,
               
               xlab = "",
               ylab = "",
               zlab = "",
               
               xlim = limits_list[[1]],
               ylim = limits_list[[2]],
               zlim = limits_list[[3]],
               
               col = data$colors,
               type = "s",
               size = 1,
               axes = FALSE,
               labels = FALSE
        )
        
        grid3d(side = c("x+", "y", "z"),
               n = 6)
        
        axes3d(edges = c("x-+", "y", "z"),
               nticks = 6
        )
        
        title3d(
          main = ifelse( input$remove_3D_labels == TRUE , "" , graph_name) ,
          xlab = ifelse( input$remove_3D_labels == TRUE , "" , "Integrated SA-B-Gal OD") ,
          ylab = ifelse( input$remove_3D_labels == TRUE , "" , "Integrated EdU intensity (AU)") ,
          zlab = ifelse( input$remove_3D_labels == TRUE , "" , "Median Nuclear Area (px^2)")
        )
        
        legend3d("right",
                 legend = unique(data$condition),
                 col =unique(data$colors),
                 pch = 16,
                 cex=1,
                 inset=c(0.02)
        )
        
        view3d(theta = -50, phi = 25, zoom = 0.9)
        
      }
      
      ## generate and save 3D graph function
      
      generate_3D_graphs <- function(data, graph_name) {
        plot_3D_median_signal(data, graph_name)
        
        file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_", graph_name, sep = "") # gets string with full path and file name for plot
        
        graph_counter <<- graph_counter + 1
        
        ### save 3D plot
        htmlwidgets::saveWidget(rglwidget(width = 800, height = 800),
                                file = file_path,
                                libdir = "HTML-dependencies",
                                selfcontained = FALSE
        )
      }
      
      ## graph with all values
      
      generate_3D_graphs(df_summary_signal_no_background, "median-signal_SABGal-EdU-NuclearArea.html")
      
      ## graphs 3D filtered for unique combinations of additional_variables
      
      if(length(additional_variables) > 0) {
        
        if(length(additional_variables) == 1) {
          filtering_vec <- df_summary_signal_no_background$additional_variable_1 %>% unique()
          
          df_names <- str_c("median-signal_SABGal-EdU-NuclearArea_", additional_variable_1,"-", filtering_vec, ".html") # names of graph files
          
          df_list <- vector("list", length = length(filtering_vec)) # filtered dfs
          
          for (i in seq_along(filtering_vec)) {
            df_list[[i]] <- filter(df_summary_signal_no_background, additional_variable_1 == filtering_vec[[i]])
          }
          
          parameters <- tibble(filtered_df = df_list, name = df_names)
          
          for (i in seq_len(nrow(parameters))) { # generate graphs
            generate_3D_graphs(parameters$filtered_df[[i]], parameters$name[[i]])
          }
          
        } else {
          
          filtering_vec_1 <- df_summary_signal_no_background$additional_variable_1 %>% unique()
          filtering_vec_2 <- df_summary_signal_no_background$additional_variable_2 %>% unique()
          
          filtering_vec <- expand_grid(additional_variable_1 = filtering_vec_1,
                                       additional_variable_2 = filtering_vec_2) # all possible combination of var 1 & var 2
          
          df_names <- str_c("median-signal_SABGal-EdU-NuclearArea_", additional_variable_1,"-", filtering_vec[[1]], "_", additional_variable_2, "-", filtering_vec[[2]], ".html")
          
          df_list <- vector("list", length = nrow(filtering_vec))
          
          for (i in seq_len(nrow(filtering_vec))) {
            df_list[[i]] <- filter(df_summary_signal_no_background, additional_variable_1 == filtering_vec[[1]][[i]] & additional_variable_2 == filtering_vec[[2]][[i]])
          }
          
          parameters <- tibble(filtered_df = df_list, name = df_names)
          
          for (i in seq_len(nrow(parameters))) {
            generate_3D_graphs(parameters$filtered_df[[i]], parameters$name[[i]])
          }
          
        }
        
      }
      
      

      # graph 3D: median area, SABGal, EdU - fold change ------------------------
      
      show_analysis_progress("graph: median-signal_SABGal-EdU-NuclearArea_fold-change.html")
      
      ## adding graph limits (for consistency across 3D plots) 
      
      variables <- c("fold_change_SABGal", "fold_change_EdU", "fold_change_Area")
      
      limits_list_fold_change <- vector("list", length = 3)
      
      for (i in seq_along(variables)) {
        limits_list_fold_change[[i]] <- generate_limits(variables[[i]])
      }
      
      adjust_limits <- function(limits_number) {
        extra_room <<- (limits_list_fold_change[[limits_number]][2] - limits_list_fold_change[[limits_number]][1])*0.03
        
        limits_list_fold_change[[limits_number]] <<- c(limits_list_fold_change[[limits_number]][1] - extra_room, limits_list_fold_change[[limits_number]][2] + extra_room)
        
        limits_list_fold_change[[limits_number]] <<- limits_list_fold_change[[limits_number]] %>% round(digits = 1)
      }
      
      for (i in seq_along(limits_list_fold_change)) {
        adjust_limits(i)
      }
      
      ## 3D plot function
      
      plot_3D_median_signal_fold_change <- function(data, graph_name) { ## working
        open3d() ## this is needed to open up a new rgl widget with each function run; otherwise, new runs will add points to previous plots
        plot3d(x = data$fold_change_SABGal,
               y = data$fold_change_EdU,
               z = data$fold_change_Area,
               
               xlab = "",
               ylab = "",
               zlab = "",
               
               xlim = limits_list_fold_change[[1]],
               ylim = limits_list_fold_change[[2]],
               zlim = limits_list_fold_change[[3]],
               
               col = data$colors,
               type = "s",
               size = 1,
               axes = FALSE,
               labels = FALSE
        )
        
        grid3d(side = c("x+", "y", "z"),
               n = 6)
        
        axes3d(edges = c("x-+", "y", "z"),
               nticks = 6
        )
        
        title3d(
          main = ifelse( input$remove_3D_labels == TRUE , "" , graph_name) ,
          xlab = ifelse( input$remove_3D_labels == TRUE , "" , "fold change SA-B-Gal") ,
          ylab = ifelse( input$remove_3D_labels == TRUE , "" , "fold change EdU") ,
          zlab = ifelse( input$remove_3D_labels == TRUE , "" , "fold change Nuclear Area")
        )
        
        legend3d("right",
                 legend = unique(data$condition),
                 col =unique(data$colors),
                 pch = 16,
                 cex=1,
                 inset=c(0.02)
        )
        
        view3d(theta = -50, phi = 25, zoom = 0.9)
        
      }
      
      ## generate and save 3D graph function
      
      generate_3D_graphs_fold_change <- function(data, graph_name) {
        plot_3D_median_signal_fold_change(data, graph_name)
        
        file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_", graph_name, sep = "") # gets string with full path and file name for plot
        
        graph_counter <<- graph_counter + 1
        
        ### save 3D plot
        htmlwidgets::saveWidget(rglwidget(width = 800, height = 800),
                                file = file_path,
                                libdir = "HTML-dependencies",
                                selfcontained = FALSE
        )
      }
      
      ## graph with all values
      
      generate_3D_graphs_fold_change(df_summary_signal_no_background, "median-signal_SABGal-EdU-NuclearArea_fold-change.html")
      
      ## graphs 3D filtered for unique combinations of additional_variables
      
      if(length(additional_variables) > 0) {
        
        if(length(additional_variables) == 1) {
          filtering_vec <- df_summary_signal_no_background$additional_variable_1 %>% unique()
          
          df_names <- str_c("median-signal_SABGal-EdU-NuclearArea_fold-change_", additional_variable_1,"-", filtering_vec, ".html") # names of graph files
          
          df_list <- vector("list", length = length(filtering_vec)) # filtered dfs
          
          for (i in seq_along(filtering_vec)) {
            df_list[[i]] <- filter(df_summary_signal_no_background, additional_variable_1 == filtering_vec[[i]])
          }
          
          parameters <- tibble(filtered_df = df_list, name = df_names)
          
          for (i in seq_len(nrow(parameters))) { # generate graphs
            generate_3D_graphs_fold_change(parameters$filtered_df[[i]], parameters$name[[i]])
          }
          
        } else {
          
          filtering_vec_1 <- df_summary_signal_no_background$additional_variable_1 %>% unique()
          filtering_vec_2 <- df_summary_signal_no_background$additional_variable_2 %>% unique()
          
          filtering_vec <- expand_grid(additional_variable_1 = filtering_vec_1,
                                       additional_variable_2 = filtering_vec_2) # all possible combination of var 1 & var 2
          
          df_names <- str_c("median-signal_SABGal-EdU-NuclearArea_fold-change_", additional_variable_1,"-", filtering_vec[[1]], "_", additional_variable_2, "-", filtering_vec[[2]], ".html")
          
          df_list <- vector("list", length = nrow(filtering_vec))
          
          for (i in seq_len(nrow(filtering_vec))) {
            df_list[[i]] <- filter(df_summary_signal_no_background, additional_variable_1 == filtering_vec[[1]][[i]] & additional_variable_2 == filtering_vec[[2]][[i]])
          }
          
          parameters <- tibble(filtered_df = df_list, name = df_names)
          
          for (i in seq_len(nrow(parameters))) {
            generate_3D_graphs_fold_change(parameters$filtered_df[[i]], parameters$name[[i]])
          }
          
        }
        
      }
      
      
      
      # Plotting SABGal and EdU graphs: END -----------------------------------
      
      show_analysis_progress("tidy IA output and analysis report")
      
      # Excel with tidy IA input ------------------------------------------------
      write_csv(tidy_data6, file = str_c(getwd(),"/",graphs_folder,"/_tidy-IA-output.csv", sep = ""))
      
      
      # Summary table -----------------------------------------------------------
      
      # table with summary of analysis results (counts, median signal, thresholds, and % of SA-B-Gal and EdU positive cells)
      summary_table <- summarise(by_well,
                                 cell_count = n(),
                                 Nuclear_Area_median = median(Nuclear_Area),
                                 SABGal_median_signal = median(SABGal),
                                 EdU_median_signal = median(EdU),
                                 fold_change_Area = Nuclear_Area_median/average_Area_Median_min,
                                 fold_change_SABGal = SABGal_median_signal/average_SABGal_Median_min,
                                 fold_change_EdU = EdU_median_signal/average_EdU_Median_min,
                                 SABGal_threshold = mean(SABGal_threshold),
                                 EdU_threshold = mean(EdU_threshold),
                                 SABGal_positive_count = sum(SABGal >= SABGal_threshold),
                                 EdU_positive_count = sum(EdU >= EdU_threshold),
                                 SABGal_positive_percentage = SABGal_positive_count / cell_count,
                                 EdU_positive_percentage = EdU_positive_count / cell_count)
      
      if (additional_variables_check == TRUE) {
        new_col_names <- summary_table %>%
          colnames() %>%
          str_replace("additional_variable_1", additional_variable_1)
        
        if (multiple_additional_variables_check == TRUE) {
          new_col_names <- new_col_names %>%
            str_replace("additional_variable_2", additional_variable_2)
        }
        
        colnames(summary_table) <- new_col_names
      }
      
      # table with input parameters
      
      input_parameters <- tibble(
        parameters = c("Image_Analyst_output_file_name",
                       "plate_template_file_name",
                       "DAPI_label_number",
                       "EdU_label_number",
                       "SABGal_label_number",
                       "EdU_threshold_percentile",
                       "SABGal_threshold_percentile",
                       "assess_cell_viability_and_senescence_markers_changes",
                       "RColorBrewer_palette",
                       "invert_colors",
                       "resolution_dpi",
                       "size_legend_title",
                       "size_legend_text",
                       "size_axis_title",
                       "size_axis_text",
                       "size_facets_text"),
        input_values = c(input$Image_Analyst_output_file_name$name,
                         input$plate_template_name$name,
                         DAPI_label_number,
                         EdU_label_number,
                         SABGal_label_number,
                         EdU_threshold_percentile,
                         SABGal_threshold_percentile,
                         assess_cell_viability_and_senescence_markers_changes,
                         RColorBrewer_palette,
                         invert_colors,
                         resolution_dpi,
                         size_legend_title,
                         size_legend_text,
                         size_axis_title,
                         size_axis_text,
                         size_facets_text)
      )
      
      # generating xcel file with summary_table
      
      sheets_list <- list("Results_Summary" = summary_table, "Input_Parameters" = input_parameters)
      
      write.xlsx(sheets_list, file = str_c(getwd(),"/",graphs_folder,"/_analysis_report.xlsx", sep = ""))
      
      
      # Cell Viability and Staining Changes: START ----------------------------------------
      
      if (input$assess_cell_viability_and_senescence_markers_changes == FALSE) {
        beep(2)
        validate(paste0("Analysis complete!
      
      The analysis results have been saved in the folder you indicated (", selected_directory,"),
      within ", graphs_folder),
                 errorClass = "analysis_completed")
      }
      
      show_analysis_progress("adjustments for optional analysis")
      
      # checks ------------------------------------------------------------------
      
      # checking for presence of additional variables
      
      if (length(additional_variables) < 1) {
        beep(1)
        validate(
          "No additional variable found for cell viability assessment

      Cell viability assessment can only be performed if the
      additional variable correspodning to a treatment is
      entered in the plate-template.csv file"
        )
      }
      
      treatment_variable <- cell_viability_variable %>%
        tolower() %>%
        str_replace_all(., "[()]", "")
      
      # checking that the cell_viability_variable matches an additional variable
      
      if (any(colnames(summary_table) == treatment_variable) == FALSE) {
        beep(1)
        validate(
          "Cell viability variable not found

        Ensure the cell viability variable entered matches
        exactly a variable entered in plate-template.csv"
        )
      }
      
      # summary table adjustments -----------------------------------------------
      
      # changing col names
      
      new_col_names <- summary_table %>%
        colnames() %>%
        str_replace(treatment_variable, "treatment_variable")
      
      colnames(summary_table) <- new_col_names
      
      # changing treatment variable into a factor
      
      summary_table$treatment_variable <- as.factor(summary_table$treatment_variable)
      
      # adding average counts per condition
      
      additional_variable_treatment <- if (treatment_variable == additional_variable_1) {"additional_variable_1"} else {"additional_variable_2"}
      
      not_condition_nor_treatment_variable <- !(str_detect(grouping_arguments_no_well, str_c("condition|",additional_variable_treatment)))
      
      if (length(grouping_arguments_no_well[not_condition_nor_treatment_variable] > 0)) {
        
        other_variable <- additional_variables[!(str_detect(additional_variables, treatment_variable))]
        
        grouping_arguments_no_well[not_condition_nor_treatment_variable] <- other_variable
      }
      
      grouping_arguments_no_well <- grouping_arguments_no_well %>%
        str_replace(additional_variable_treatment, "treatment_variable")
      
      
      df_average <- summary_table %>%
        group_by(!!!syms(grouping_arguments_no_well)) %>%
        summarise(average_cell_count = mean(cell_count))
      
      summary_table <- summary_table %>%
        left_join(df_average)
      
      # summary table w/o background
      
      summary_table_signal <- summary_table %>%
        filter_not_background()
      
      
      # graphs settings adjustments ---------------------------------------------
      
      # variable for facet_grid cols
      
      facet_grid_cols <- additional_variables[-grep(treatment_variable, additional_variables)]
      
      # number of rows and cols for facets_grid
      
      grid_col_n <- if (length(facet_grid_cols) == 0) {1} else {
        summary_table_signal[[facet_grid_cols]] %>%
          unique() %>%
          length()
      }
      
      default_width <- 300 * (resolution_dpi/72) + 180 * grid_col_n * (resolution_dpi/72)
      
      grid_row_n <- summary_table_signal$condition %>% unique() %>% length()
      
      default_height <- 50 * (resolution_dpi/72) + 200 * grid_row_n * (resolution_dpi/72)
      
      
      
      # graphs boxplot cell counts ------------------------------------------------------
      
      show_analysis_progress("graphs: changes_cell-counts")
      
      # ggplot function
      
      plot_boxplot <- function(data = summary_table_signal, x = summary_table_signal$treatment_variable, y, xlab = cell_viability_variable, ylab) {
        ggplot(data,
               aes(x = x,
                   y = y,
                   color = condition)) +
          geom_boxplot(linewidth = 0.25) +
          geom_point(size = 3,
                     shape = 21,
                     color = "black",
                     aes(fill = condition)) +
          labs(
            color = "Condition",
            fill = "Condition",
            x = str_replace_all(xlab, "_", " "),
            y = ylab
          ) +
          {if (length(facet_grid_cols) == 0)
            facet_grid(rows = vars(condition))
            else
              facet_grid(rows = vars(condition), cols = vars(!!sym(facet_grid_cols)))} +
          guides(colour = guide_legend(override.aes = list(size = 3))) +
          color_scale_conditions +
          fill_scale_conditions
      }
      
      # graph counts all cells
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_cell-counts.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_boxplot(y = summary_table_signal$cell_count, ylab = "Cell Counts")
      
      default_png()# saves the plot above to PNG
      
      # graph counts SA-B-Gal-positive cells
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_cell-counts_SABGal-positive.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_boxplot(y = summary_table_signal$SABGal_positive_count, ylab = "SA-\U03B2-Gal+ Cell Counts")
      
      default_png()# saves the plot above to PNG
      
      # graph counts SA-B-Gal-positive cells
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_cell-counts_EdU-positive.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_boxplot(y = summary_table_signal$EdU_positive_count, ylab = "EdU+ Cell Counts")
      
      default_png()# saves the plot above to PNG
      
      # graphs boxplot proportions ------------------------------------------------------
      
      show_analysis_progress("graphs: changes_percentage")
      
      # summary_table_proportions
      
      max_average_cell_counts <- summary_table_signal %>%
        group_by(condition) %>%
        summarise(max_average_cell_count = max(average_cell_count))
      
      summary_table_signal <- left_join(summary_table_signal, max_average_cell_counts)
      
      summary_table_signal <- summary_table_signal %>%
        mutate(percent_viable = cell_count / max_average_cell_count)
      
      # ggplot function
      
      upper_limit <- if (1.1 > max(summary_table_signal$percent_viable)) {1.1} else {max(summary_table_signal$percent_viable)}
      
      plot_proportions <- function(data = summary_table_signal, x = summary_table_signal$treatment_variable, y, xlab = cell_viability_variable, ylab) {
        ggplot(data,
               aes(x = x,
                   y = y,
                   color = condition)) +
          geom_hline(yintercept = 1,
                     color = "gray",
                     linewidth = 1,
                     alpha = 1/2) +
          geom_boxplot(position = "identity",
                       linewidth = 0.25) +
          geom_point(size = 3,
                     shape = 21,
                     color = "black",
                     aes(fill = condition)) +
          labs(
            color = "Condition",
            fill = "Condition",
            x = str_replace_all(xlab, "_", " "),
            y = ylab
          ) +
          {if (length(facet_grid_cols) == 0)
            facet_grid(rows = vars(condition))
            else
              facet_grid(rows = vars(condition), cols = vars(!!sym(facet_grid_cols)))} +
          scale_y_continuous(breaks = seq(0.0, upper_limit, by = 0.2),
                             limits = c(0, upper_limit),
                             labels = percent) +
          guides(colour = guide_legend(override.aes = list(size = 3))) +
          color_scale_conditions +
          fill_scale_conditions
      }
      
      # graph proportion all cells
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_percentage_viability.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_proportions(y = summary_table_signal$percent_viable, ylab = "Viability")
      
      default_png()# saves the plot above to PNG
      
      # graph proportions SA-B-Gal positive cells
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_percentage_SABGal-positive.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_proportions(y = summary_table_signal$SABGal_positive_percentage, ylab = "% SA-\U03B2-Gal+ Cells")
      
      default_png()# saves the plot above to PNG
      
      # graph proportions EdU positive cells
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_percentage_EdU-positive.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_proportions(y = summary_table_signal$EdU_positive_percentage, ylab = "% EdU+ Cells")
      
      default_png()# saves the plot above to PNG
      
      # graphs boxplots median signal ----------------------------------------------------
      
      show_analysis_progress("graphs: changes_median-signal")
      
      # graph SA-B-Gal median values
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_median-signal_SABGal.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_boxplot(y = summary_table_signal$SABGal_median_signal, ylab = "Median SA-\U03B2-Gal OD")
      
      default_png() # saves the plot above to PNG
      
      # graph EdU median values
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_median-signal_EdU.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_boxplot(y = summary_table_signal$EdU_median_signal, ylab = "Median EdU Signal (AU)")
      
      default_png() # saves the plot above to PNG
      
      
      # graph boxplot nuclear area median signal --------------------------------
      
      show_analysis_progress("graphs: changes_median-area")
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_median-area.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_boxplot(y = summary_table_signal$Nuclear_Area_median, ylab = expression(paste("Median Nuclear Area (px"^"2", ")")))
      
      default_png() # saves the plot above to PNG
      
      
      # graphs scatterplot changes median values --------------
      
      show_analysis_progress("graphs: median-signal_SABGal-and-EdU")
      
      # axis ticks
      
      upper_limit_SABGal_median_changes <- summary_table_signal$SABGal_median_signal %>% max() %>% log10()
      
      x_axis_ticks_median_signal <- seq(from = 0, to = upper_limit_SABGal_median_changes) %>%
        10^.
      
      upper_limit_EdU_median_changes <- summary_table_signal$EdU_median_signal %>% max() %>% log10()
      
      y_axis_ticks_median_signal <- seq(from = 0, to = upper_limit_EdU_median_changes) %>%
        10^.
      
      # ggplot function
      
      plot_changes_median <- function(data = summary_table_signal, x, y, xlab, ylab) {
        ggplot(data,
               aes(x = x,
                   y = y,
                   fill = treatment_variable)) +
          geom_point(size = 3,
                     color = "black",
                     shape = 21) +
          scale_x_continuous(name = xlab,
                             trans = pseudo_log_trans(base = 10),
                             breaks = x_axis_ticks_median_signal
          ) +
          scale_y_continuous(name = ylab,
                             trans = pseudo_log_trans(base = 10),
                             breaks = y_axis_ticks_median_signal
          ) +
          labs(
            fill = cell_viability_variable %>% str_replace_all("_", " "),
            x = xlab,
            y = ylab
          ) +
          {if (length(facet_grid_cols) == 0)
            facet_grid(rows = vars(condition))
            else
              facet_grid(rows = vars(condition), cols = vars(!!sym(facet_grid_cols)))} +
          guides(colour = guide_legend(override.aes = list(size = 3))) +
          scale_fill_brewer(palette = "Greys")
      }
      
      # graph scatterplot changes in median values
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_median-signal_SABGal-and-EdU.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_changes_median(x = summary_table_signal$SABGal_median_signal,
                          y = summary_table_signal$EdU_median_signal,
                          xlab = "Median SA-\U03B2-Gal OD",
                          ylab = "Median EdU Signal (AU)")
      
      default_png()# saves the plot above to PNG
      
      
      # graphs scatterplot changes in median values 3D (w/ Nuclear Area) --------
      
      show_analysis_progress("graphs: changes_median-signal_SABGal-EdU-NuclearArea")
      
      # adding colors
      
      color_values <- RColorBrewer::brewer.pal(length(unique(summary_table_signal$treatment_variable)), "Greys")
      if (length(unique(summary_table_signal$treatment_variable)) < 3) {
        color_values <- color_values[-3] 
      }
      
      plot3D_colors_changes <- tibble(
        "colors" = color_values,
        "treatment_variable" = sort(unique(summary_table_signal$treatment_variable))
      )
      
      summary_table_signal <- left_join(summary_table_signal, plot3D_colors_changes)
      
      ## 3D plot function
      
      plot_3D_median_signal_changes <- function(data, graph_name) { ## working
        open3d() ## this is needed to open up a new rgl widget with each function run; otherwise, new runs will add points to previous plots
        plot3d(x = data$SABGal_median_signal,
               y = data$EdU_median_signal,
               z = data$Nuclear_Area_median,
               
               xlab = "",
               ylab = "",
               zlab = "",
               
               xlim = limits_list[[1]],
               ylim = limits_list[[2]],
               zlim = limits_list[[3]],
               
               col = data$colors,
               type = "s",
               size = 1,
               axes = FALSE,
               labels = FALSE
        )
        
        grid3d(side = c("x+", "y", "z"),
               n = 6)
        
        axes3d(edges = c("x-+", "y", "z"),
               nticks = 6
        )
        
        title3d(
          main = ifelse( input$remove_3D_labels == TRUE , "" , graph_name) ,
          xlab = ifelse( input$remove_3D_labels == TRUE , "" , "Integrated SA-B-Gal OD") ,
          ylab = ifelse( input$remove_3D_labels == TRUE , "" , "Integrated EdU intensity (AU)") ,
          zlab = ifelse( input$remove_3D_labels == TRUE , "" , "Median Nuclear Area (px^2)")
        )
        
        legend3d("right",
                 legend = unique(data$treatment_variable), ## legend changed to show treatment variable
                 col =unique(data$colors),
                 pch = 16,
                 cex=1,
                 inset=c(0.02)
        )
        
        view3d(theta = -50, phi = 25, zoom = 0.9)
        
      }
      
      
      # generate 3D graphs function
      
      generate_3D_graphs_changes <- function(data, graph_name) {
        plot_3D_median_signal_changes(data, graph_name)
        
        file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_", graph_name, sep = "") # gets string with full path and file name for plot
        
        graph_counter <<- graph_counter + 1
        
        ### save 3D plot
        htmlwidgets::saveWidget(rglwidget(width = 800, height = 800),
                                file = file_path,
                                libdir = "HTML-dependencies",
                                selfcontained = FALSE
        )
      }
      
      ## graphs 3D filtered for unique conditions (+ other additional variable if present)
      
      if(length(facet_grid_cols) == 0) { # filtering for condition
        
        filtering_vec <- summary_table_signal$condition %>% unique()
        
        df_names <- str_c("changes_median-signal_SABGal-EdU-NuclearArea_", treatment_variable,"-", filtering_vec, ".html") # names of graph files
        
        df_list <- vector("list", length = length(filtering_vec)) # filtered dfs
        
        for (i in seq_along(filtering_vec)) {
          df_list[[i]] <- filter(summary_table_signal, condition == filtering_vec[[i]])
        }
        
        parameters <- tibble(filtered_df = df_list, name = df_names)
        
        for (i in seq_len(nrow(parameters))) { # generate graphs
          generate_3D_graphs_changes(parameters$filtered_df[[i]], parameters$name[[i]])
        }
        
      } else { # filtering for condition + other additional variable
        
        filtering_vec_1 <- summary_table_signal$condition %>% unique()
        filtering_vec_2 <- summary_table_signal[[facet_grid_cols]] %>% unique()
        
        filtering_vec <- expand_grid(condition = filtering_vec_1,
                                     facet_grid_cols = filtering_vec_2)  # all possible combination of conditino and other additional variable
        
        df_names <- str_c("changes_median-signal_SABGal-EdU-NuclearArea_", treatment_variable,"-", filtering_vec[[1]], "_", facet_grid_cols, "-", filtering_vec[[2]], ".html")
        
        df_list <- vector("list", length = nrow(filtering_vec))
        
        for (i in seq_len(nrow(filtering_vec))) {
          df_list[[i]] <- filter(summary_table_signal, condition == filtering_vec[[1]][[i]] & facet_grid_cols == filtering_vec[[2]][[i]])
        }
        
        parameters <- tibble(filtered_df = df_list, name = df_names)
        
        for (i in seq_len(nrow(parameters))) {
          generate_3D_graphs_changes(parameters$filtered_df[[i]], parameters$name[[i]])
        }
        
      }
      
      
      # graphs scatterplot changes in median values 3D - fold change (w/ Nuclear Area) --------
      
      show_analysis_progress("graphs: changes_median-signal_SABGal-EdU-NuclearArea_fold-change")
      
      ## 3D plot function
      
      plot_3D_median_signal_relative_changes <- function(data, graph_name) { ## working
        open3d() ## this is needed to open up a new rgl widget with each function run; otherwise, new runs will add points to previous plots
        plot3d(x = data$fold_change_SABGal,
               y = data$fold_change_EdU,
               z = data$fold_change_Area,
               
               xlab = "",
               ylab = "",
               zlab = "",
               
               xlim = limits_list_fold_change[[1]],
               ylim = limits_list_fold_change[[2]],
               zlim = limits_list_fold_change[[3]],
               
               col = data$colors,
               type = "s",
               size = 1,
               axes = FALSE,
               labels = FALSE
        )
        
        grid3d(side = c("x+", "y", "z"),
               n = 6)
        
        axes3d(edges = c("x-+", "y", "z"),
               nticks = 6
        )
        
        title3d(
          main = ifelse( input$remove_3D_labels == TRUE , "" , graph_name) ,
          xlab = ifelse( input$remove_3D_labels == TRUE , "" , "fold change SA-B-Gal") ,
          ylab = ifelse( input$remove_3D_labels == TRUE , "" , "fold change EdU") ,
          zlab = ifelse( input$remove_3D_labels == TRUE , "" , "fold change Nuclear Area")
        )
        
        legend3d("right",
                 legend = unique(data$treatment_variable), ## legend changed to show treatment variable
                 col =unique(data$colors),
                 pch = 16,
                 cex=1,
                 inset=c(0.02)
        )
        
        view3d(theta = -50, phi = 25, zoom = 0.9)
        
      }
      
      
      # generate 3D graphs function
      
      generate_3D_graphs_relative_changes <- function(data, graph_name) {
        plot_3D_median_signal_relative_changes(data, graph_name)
        
        file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_", graph_name, sep = "") # gets string with full path and file name for plot
        
        graph_counter <<- graph_counter + 1
        
        ### save 3D plot
        htmlwidgets::saveWidget(rglwidget(width = 800, height = 800),
                                file = file_path,
                                libdir = "HTML-dependencies",
                                selfcontained = FALSE
        )
      }
      
      ## graphs 3D filtered for unique conditions (+ other additional variable if present)
      
      if(length(facet_grid_cols) == 0) { # filtering for condition
        
        filtering_vec <- summary_table_signal$condition %>% unique()
        
        df_names <- str_c("changes_median-signal_SABGal-EdU-NuclearArea_fold-change_", treatment_variable,"-", filtering_vec, ".html") # names of graph files
        
        df_list <- vector("list", length = length(filtering_vec)) # filtered dfs
        
        for (i in seq_along(filtering_vec)) {
          df_list[[i]] <- filter(summary_table_signal, condition == filtering_vec[[i]])
        }
        
        parameters <- tibble(filtered_df = df_list, name = df_names)
        
        for (i in seq_len(nrow(parameters))) { # generate graphs
          generate_3D_graphs_relative_changes(parameters$filtered_df[[i]], parameters$name[[i]])
        }
        
      } else { # filtering for condition + other additional variable
        
        filtering_vec_1 <- summary_table_signal$condition %>% unique()
        filtering_vec_2 <- summary_table_signal[[facet_grid_cols]] %>% unique()
        
        filtering_vec <- expand_grid(condition = filtering_vec_1,
                                     facet_grid_cols = filtering_vec_2)  # all possible combination of conditino and other additional variable
        
        df_names <- str_c("changes_median-signal_SABGal-EdU-NuclearArea_fold-change_", treatment_variable,"-", filtering_vec[[1]], "_", facet_grid_cols, "-", filtering_vec[[2]], ".html")
        
        df_list <- vector("list", length = nrow(filtering_vec))
        
        for (i in seq_len(nrow(filtering_vec))) {
          df_list[[i]] <- filter(summary_table_signal, condition == filtering_vec[[1]][[i]] & facet_grid_cols == filtering_vec[[2]][[i]])
        }
        
        parameters <- tibble(filtered_df = df_list, name = df_names)
        
        for (i in seq_len(nrow(parameters))) {
          generate_3D_graphs_relative_changes(parameters$filtered_df[[i]], parameters$name[[i]])
        }
        
      }
      
      
      
      
      # graphs scatterplot changes percentages ---------------------------------
      
      show_analysis_progress("graphs: changes_percentage_SABGal-and-EdU")
      
      # ggplot function
      
      plot_changes_percentages <- function(data = summary_table_signal) {
        ggplot(data, aes(SABGal_positive_percentage, EdU_positive_percentage)) +
          geom_point(
            shape = 21,
            color = "black",
            aes(fill = treatment_variable),
            size = 3) +
          {if (length(facet_grid_cols) == 0)
            facet_grid(rows = vars(condition))
            else
              facet_grid(rows = vars(condition), cols = vars(!!sym(facet_grid_cols)))} +
          scale_x_continuous(
            limits = c(0,1),
            labels = percent, name = "% SA-\u03B2-Gal+ cells",
            breaks = seq(0, 1, by = 0.2)) +
          scale_y_continuous(
            limits = c(0,1),
            labels = percent, name = "% EdU+ cells",
            breaks = seq(0, 1, by = 0.2)) +
          labs(fill = cell_viability_variable %>% str_replace_all("_", " ")) +
          scale_fill_brewer(palette = "Greys") +
          guides(color = guide_legend(override.aes = list(alpha = 1,
                                                          size = 3))) # to ensure colors in legend are visible
      }
      
      # graph scatterplot changes in percentage values
      
      file_path <- str_c(getwd(),"/",graphs_folder,"/", ifelse(graph_counter %/% 10 < 1, str_c("0", graph_counter), graph_counter), "_changes_percentage_SABGal-and-EdU.png", sep = "") # gets string with full path and file name for plot
      
      graph_counter <- graph_counter + 1
      
      plot_changes_percentages()
      
      default_png()# saves the plot above to PNG
      
      # Cell Viability Assessment: END ------------------------------------------
      
      
      # Beep at end --------------------------------------------------------------
      
      beep(2)
      validate(paste0("Analysis complete!
      
      The analysis results have been saved in the folder you indicated (", selected_directory,"),
      within ", graphs_folder),
                 errorClass = "analysis_completed")
      
    })
  })
  
  output$analysis_output <- renderText({
    analysis_output()
  })
}

# Run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)


# Notes -------------------------------------------------------------------


