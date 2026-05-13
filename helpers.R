
# Helper functions ####
### Clean weights data ####
#' 
#' 
#'
clean_weights <- function(data){

  weights <- data |> 
    dplyr::relocate(report_year, .before = dplyr::everything()) |> 
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", "weight_year", "weightings")) |>  
    tidyr::pivot_longer(cols = 2:dplyr::last_col(),
                        names_to = "factor", 
                        values_to = "weight") |> 
    dplyr::mutate(weight = as.integer(weight), 
           factor = str_extract(factor, pattern = "(?<=[:punct:])[:alpha:]+")) |> # extract words/letters that are preceded by a punctuation
    tidyr::drop_na(weight) #|>
    # dplyr::summarise(avg_weight = round(
    #                                     mean(weight, na.rm = T), 
    #                                     2),
    #                 .by = c(report_year, factor)) 

  return(weights)
}


### Render Report function #####
#'
#' 
#' 
## create render report function 
render_report <- function(input, output, params) {
  # render the report by rendering the RMD file
  rmarkdown::render(input,
    output_file = output,
    params = params,
    envir = new.env(parent = globalenv())
  )
}