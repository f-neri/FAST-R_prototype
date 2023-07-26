
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


# Functions ---------------------------------------------------------------

show_analysis_progress <<- function(file_name_generated = "") {
  percentage_progress <<- percentage_progress + 1/34*100 
  incProgress(1,
              detail = str_c("Progress: ", round(percentage_progress, digits = 0), "%\n
                                   Generating ",file_name_generated))
}




# START: UI ---------------------------------------------------------------

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
      fileInput("Image_Analyst_output_file_name", label = "Image Analyst output file name", accept = ".xlsx"),
      fileInput("plate_template_name", label = "Plate template file name",  accept = ".csv"),
      br(),
      h3(em("Additional Parameters")),
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
      numericInput("resolution_dpi", label = "Graphs resolution (dpi)", value = 600), # if changed to selectInput gives issue
      helpText("low: 72; medium: 150; high: 300; ultra high; 600"),
      br(),
      p(strong("Character fonts for graph text")),
      numericInput("size_legend_title", label = "Legend title", value = 14),
      numericInput("size_legend_text", label = "Legend text", value = 12),
      numericInput("size_axis_title", label = "Axis title", value = 14),
      numericInput("size_axis_text", label = "Axis text", value = 12),
      numericInput("size_facets_text", label = "Facet text", value = 14),
      br(),
      h3(em("Save Analysis")),
      directoryInput('directory', label = 'select a folder to save files in'),
      br(),
      h3(em("Run Analysis")),
      br(),
      actionButton("run_button", label = "Run")
      ),
    
    
    mainPanel(
      h1("Instructions"),
      br(),
      p("Follow the instructions below to perform run the FAST Data Analysis app:"),
      br(),
      textOutput("test_output")
      )
  )
)


# START: Server -----------------------------------------------------------

server <- function(input, output, session) {
  
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
  
  
  operations_results <- eventReactive(input$run_button, {

# Saving input values to objects ------------------------------------------
    
    ## input files
    Image_Analyst_output_file_name <<- input$Image_Analyst_output_file_name$datapath
    plate_template_name <<- input$plate_template_name$datapath
    
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

    setwd(selected_directory) # setting wd based on directory entered as input
    
    percentage_progress <<- 0 # sets progress bar initial value to 0
    
    withProgress(min = 0, max = 34, message = "Analysis is running", value = 0, {
      
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
        Sys.sleep(2)
        stop(
          "The metadata entered in plate-template must contain the \"Condition\" variable"
        )}
      
      ## check that the variables contained in plate-template are limited to
      ## condition, and up to 2 additional variable (e.g. serum and/or drug treatment)
      additional_variables <- colnames(plate_metadata) %>%
        .[-grep(pattern = "well|condition", .)]
      
      if (length(additional_variables) > 2) {
        beep(1)
        Sys.sleep(2)
        stop(
          "The metadata entered in plate-template is not acceptable.

The only metadata that can be entered in the plate-template file are
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
      
      return(file_path)
    })
  })
  
  output$test_output <- renderPrint({
    operations_results()
  })
}

# Run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)


# Notes -------------------------------------------------------------------


