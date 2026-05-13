# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

# Load packages and functions
library(surveydown)
library(shiny)
library(here)
library(tidyverse)
library(gt)
source(here("helpers.R"))
# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# Set ignore = TRUE in the following code to ignore
# the connection settings and avoid connecting to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(env_file = ".env",
                    ignore = FALSE)

# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {
    # Define any conditional skip logic here (skip to page if a condition is true)
  # sd_skip_forward()

  # Define any conditional display logic here (show a question if a condition is true)
  # sd_show_if()

  data <- sd_get_data(db, table = "rp-weights", refresh_interval = 60)
  # data <- data_reactive() |> as.data.frame()
  
  # observe({
  # year1 <- all_data$report_year
  year <- reactive({ 
    sd_value(id = "report_year", type = "value") |> as.character()
  })

  levels <- c("biomass", "recruitment", "climate", "commercial", "recreational")
  # year <- as.character(lubridate::year(lubridate::now()))
  weight_data <- reactive({
    data() |> 
      clean_weights() |> 
      filter(report_year == year()) |> 
      mutate(factor = factor(factor, levels = levels)) |> 
      arrange(factor)
  })
  # })

  output$weight_tbl <- render_gt({
    weight_data() |> 
      gt(rowname_col = "report_year", 
         groupname_col = "factor") |> 
      tab_stubhead(label = "Action Year") |>
      # summary_rows(
      #   groups = 
      #   fns = list(
      #     Average ~ mean(.)
      #   ), 
      #   fmt = ~fmt_number(.)
      # ) |>
      cols_label(
        weight = "Weight given"
      ) |> 
      cols_width(everything() ~ px(125))
  })



  # Report #### ==========================
  report_path <- tempfile(fileext = ".Rmd")

  ## copy the RMD file in the repo to the temporary file location and overwrite if already existing
  file.copy("rp_report_template.Rmd", report_path, overwrite = TRUE)
    
  report_name <- reactive({
    # stringr::str_replace(stock(), pattern = "[:space:]", replace = "_") |> 
    stringr::str_c(year(),"_rp-weighting-report", ".pdf", sep = "")
  })

  output$report <- downloadHandler(
    filename = function() {
        report_name()
      }, 
    content = function(file) {
      # Use withProgress to show a progress bar
      withProgress(message = "Creating Report: ", value = 0, {

      # Stage 1: Increment progress
      incProgress(0.1, detail = "Collecting inputs...")

      # Generate the Quarto params
      params <- list(year = year(), 
                    weights = weight_data()
                    )
      # debug params
      print("Parameters for RMD render:")
      print(params)

      # debug file path
      # print(paste("Temporary output file path:", temp_output_dir))
      print(paste("file name path:", report_name()))
      print(paste("Final output file path:", file))
      # quarto_file <- normalizePath(here("draft_report.qmd", mustWork = TRUE))
      # Temporarily switch to a temp directory to avoid write permission issues
      # and ensure unique file generation for concurrent users
      incProgress(0.2, detail = "Building...")
          
          tryCatch({
            
            render_report(input = report_path, output = file, params = params)
            
            
            incProgress(0.95, detail = "Downloading report...")
            
            # Copy the generated file to the correct location for download
            # file.copy(report_name(), file, overwrite = TRUE)
            
          }, error = function(e) {
            
            print(paste("Error generating RMD report:", e$message))
            
          })

})
    })

   # Run surveydown server and define database =======================================
  sd_server(db = db)

}


# Launch the app
shiny::shinyApp(ui = ui, server = server)
