
# --- Data processing helper functions ----------------------------------------

null_or_blank <- function(x) {
  return(is.null(x) || (x == ""))
}


split_values <- function (df, col_name) {
  
  # Split comma-separated values into separate table with one per row and
  # any colon-separated values split into main and sub-values
  
  # Split comma-separated field and trim whitespace
  df_out <- df %>%
    drop_na(col_name) %>%
    select(incident_id, col_name) %>%
    separate_rows(col_name, sep = ",") %>%
    mutate_if(is.character, str_trim)
  
  # Check for colon-separated values
  sub_count <- df %>%
    pull(col_name) %>%
    str_count(":") %>%
    sum(na.rm = TRUE)
  
  # Split any colon-separated values into main and sub-values
  if (sub_count > 0) {
    df_out <- df_out %>%
      separate(col_name, 
               c(paste(col_name, "main", sep = "_"), 
                 paste(col_name, "sub", sep = "_")), 
               sep = ": ",
               remove = FALSE)
  }
  
  return(df_out)
  
}

get_ccmf <- function() {
  
  # Read CCMF data from downloaded CSV file and transform comma-separated fields
  # into separate tables which can be joined with the main data table to create
  # a unified data model for filtering and visualization
  
  # Initialize empty list to store data tables and metadata
  output <- list(main = NULL)
  
  # Main data table
  output$main <- read.csv("./data/ccmf_main.csv", encoding = "UTF-8") %>%
    
    # Drop empty rows
    drop_na(incident_id) %>%
    
    # Convert incident date to date format, and fill missing gender with N/A
    mutate(
      incident_date = as_date(incident_date),
      gender = replace_na(gender, "N/A")
    )
    
  
  # Reference table for categories and definitions
  output$info_ref <- read.csv(
    "./data/ccmf_categories.csv",
    encoding = "UTF-8",
    check.names = FALSE,
    colClasses = c("character", "character", "character", "character",
                   "character", "character")
  ) %>%
    mutate(
      category_main_abbrev = coalesce(category_main_abbrev, category_main),
      category_sub_abbrev = coalesce(category_sub_abbrev, category_sub)
    )

  # Reference table for lat, lon coordinates of cities
  output$city_ref <- read.csv("./data/locations.csv", encoding = "UTF-8")
  
  
  # --- Transform data --------------------------------------------------------
  
  # Split comma-separated fields into separate tables which can be joined
  # with the main table on incident_id
  
  # ***** Temporary -- for testing different Context hierarchy
  output$context2 <- output$main %>%
    mutate(context2 = str_replace_all(context, "Other industries/sectors: ", "")) %>%
    split_values("context2")
  
  # *********************************************************
  
  output$gender <- output$main %>% split_values("gender")
  output$incident_category <- output$main %>% split_values("incident_category")
  output$incident_type <- output$main %>% split_values("incident_type")
  output$context <- output$main %>% split_values("context")
  output$identity_based <- output$main %>% split_values("identity_based")
  
  # Since main and sub- ethnic communities are in two separate columns, they
  # have to be handled differently
  ethnicity1 <- output$main %>% 
    split_values("ethnic_community") %>%
    mutate(
      ethnic_community_main = ethnic_community,
      ethnic_community_sub = NA
    ) %>%
    select(!ethnic_community)
  
  ethnicity2 <- output$main %>% 
    split_values("ethnic_sub_community") %>%
    rename("ethnic_community_sub" = "ethnic_sub_community") %>%
    left_join(
      output$info_ref %>%
        filter(field == "ethnic_community") %>%
        drop_na(category_sub) %>%
        rename("ethnic_community_main" = "category_main",
               "ethnic_community_sub" = "category_sub") %>%
        select(ethnic_community_main, ethnic_community_sub)
    )
  
  output$ethnic_community <- rbind(ethnicity1, ethnicity2)
  
  # Minimum and maximum dates in the data (rounded to the month)
  dates <- output$main$incident_date
  output$min_date <- floor_date(min(dates), unit = "month")
  output$max_date <- ceiling_date(max(dates), unit = "month") - days(1)
  
  return(output)
}
