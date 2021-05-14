updateChoices <- function(ccmf, provinces, session, verbose = TRUE) {
  # Update user input choices to reflect the values in the data
  
  if (verbose) print("*** Updating user input choices ***")
  
  # Date range
  updateDateRangeInput(
    session,
    "summary_date_range",
    start = ccmf$min_date,
    end = ccmf$max_date,
    min = ccmf$min_date,
    max = ccmf$max_date
  )
  
  # Province
  updateSelectInput(
    session,
    "summary_province",
    choices = c("All", sort(c(provinces$province, "N/A")))
  )

  # Other inputs
  var_names <- c("year", "city", "incident_category",
                 "incident_type", "context", "ethnic_community",
                 "identity_based", "gender")

  for (var_name in var_names) {

    # Name of variable in user input list
    input_name <- paste("summary", var_name, sep = "_")

    # Name of column to extract to get values for choices
    if (var_name == "year") {
      col_name <- "incident_year"
    }
    else if (var_name == "city") {
      col_name <- "loc_full"
    }
    else if (var_name %in% c("incident_category", "ethnic_community")) {
      col_name <- paste(var_name, "main", sep = "_")
    }
    else {
      col_name <- var_name
    }

    # Extract values from the data
    if (var_name %in% c("year", "city")) {
      vals <- ccmf$main %>%
        drop_na(col_name) %>%
        pull(col_name)
    }
    else {
      vals <- ccmf[[var_name]] %>%
        pull(col_name)
    }

    # Create list of sorted, unique values for user choices
    vals <- sort(unique(vals))

    if (var_name == "year") {
      vals <- c("All", vals, "Custom date range")
    }

    # Update choices
    updateSelectInput(session, input_name, choices = vals)
  }
}

updateCityChoices <- function(ccmf, session, selected_province, verbose = TRUE) {
  # Update city choices to reflect selected province
  
  if (verbose) print("*** Updating city choices ***")

  df_sub <- ccmf$main
  if (selected_province != "All") {
    df_sub <- df_sub %>% filter(province == selected_province)
  }
  vals <- sort(unique(df_sub %>%drop_na("city") %>% pull("loc_full")))
  
  updateSelectInput(session, "summary_city", choices = vals)
}

selectedIds <- function(ccmf, opts, verbose = TRUE) {
  # Return array of incident IDs for articles matching selected user inputs
  
  if (verbose) print("*** Computing incident IDs for selected options ***")
  
  df_sub <- ccmf$main %>%
    {
      if(!(opts$year %in% c("All", "Custom date range")))
        filter(., incident_year == as.integer(opts$year))
      else .
    } %>%
    {
      if(opts$year == "Custom date range")
        filter(., incident_date >= opts$date_range[1],
               incident_date <= opts$date_range[2])
      else .
    } %>%
    {
      if(opts$province != "All")
        filter(., province == opts$province)
      else .
    } %>%
    {
      if(!null_or_blank(opts$city))
        filter(., loc_full %in% opts$city)
      else .
    }
  
  var_names <- c("incident_category", "incident_type", "context", 
                 "ethnic_community", "identity_based", "gender")
  
  for (var_name in var_names) {
    
    if (var_name %in% c("incident_category", "ethnic_community")) {
      col_name <- paste(var_name, "main", sep = "_")
    }
    else {
      col_name <- var_name
    }
    
    if(!null_or_blank(opts[[var_name]])) {
      df_sub <- df_sub %>%
        select(!all_of(var_name)) %>%
        inner_join(
          ccmf[[var_name]] %>% filter(.[[col_name]] %in% opts[[var_name]]),
          by = "incident_id"
        )
    }
  }
  
  df_sub %>% pull(incident_id)
}

selectedFilters <- function(opts) {
  # Return string summarizing all filters applied to the data (NULL if none)
  
  filters_str <- NULL
  
  # Separator for different variables
  var_sep <- " | "
  
  # Year / date range
  if (opts$year == "Custom date range") {
    filters_str <- paste(
      "<em>Time period:</em>", 
      paste(opts$date_range, collapse = " - ")
    )
  } else if (opts$year != "All") {
    filters_str <- paste("<em>Time period:</em>", opts$year)
  }
  
  # Province (single-select)
  if (opts$province != "All") {
    val <- paste("<em>Province: </em>", opts$province)
    if (is.null(filters_str)) {
      filters_str <- val
    } else {
      filters_str <- paste(filters_str, val, sep = var_sep)
    }
  }
  
  # Other options (multi-select)
  opt_labels <- list(
    "city" = "Municipality",
    "ethnic_community" = "Ethnic communities", 
    "identity_based" = "Identity-based intersectionalities", 
    "gender" = "Gender",
    "incident_category" = "Criminal vs. non-criminal", 
    "incident_type" = "Type of incident", 
    "context" = "Incident occurred in the context of"
  )
  
  for (name in names(opt_labels)) {
    if(!null_or_blank(opts[[name]])) {
      vals <- paste(
        paste0("<em>", opt_labels[[name]], "</em>"),
        paste(opts[[name]], collapse = ", "),
        sep = ": "
      )
      
      if (is.null(filters_str)) {
        filters_str <- vals
      } else {
        filters_str <- paste(filters_str, vals, sep = var_sep)
      }
    }
  }
  
  # Construct HTML content for alert box
  if (is.null(filters_str)) {
    content <- span(
      class = "alert-intro", "You are currently viewing all data.")
  } else {
    content <- tagList(
      span(class = "alert-intro", "You are currently viewing data for: "),
      span(HTML(filters_str))
    )
  }
  
  # Alert box
  div(
    class = "alert-custom",
    role = "alert",
    content
  )
  
}

userSelections <- function(ccmf, opts) {
  list(
    ids = selectedIds(ccmf, opts),
    filters_alert = selectedFilters(opts)
  )
}


dateRange <- function(ccmf, selected_ids, selected_year, selected_date_range) {
  # Minimum and maximum dates for timeseries plot
  
  if (selected_year == "All") {
    c(ccmf$min_date, ccmf$max_date)
  } else if (selected_year == "Custom date range") {
    selected_date_range
  } else {
    c(ymd(paste(selected_year, "-01-01", sep="")),
      min(ccmf$max_date, ymd(paste(selected_year, "-12-31", sep=""))
      )
    )
  }
  
}

provinceTotals <- function(ccmf, selected_ids, provinces, verbose = TRUE) {
  if (verbose) print("*** Computing province totals ***")
  ccmf$main %>%
    filter(incident_id %in% selected_ids) %>%
    group_by(prov_code) %>% 
    summarize(incidents = n()) %>%
    full_join(provinces, by = "prov_code") %>%
    replace_na(list(incidents = 0)) %>%
    mutate(
      incidents_per100k = 1e5 * incidents / population,
      label = sprintf(
        "<strong>%s</strong><br/>Incidents: %d<br>Incidents per 100k: %.4f",
        province,
        incidents,
        incidents_per100k
      ) %>% lapply(htmltools::HTML)
    )
}

provinceSpTotals <- function(prov_totals, prov_geom, verbose = TRUE) {
  if (verbose) print("*** Merging province totals with spatial data ***")
  merge(prov_geom, prov_totals, by.x="postal", by.y="prov_code")
}

cityTotals <- function(ccmf, selected_ids, cities_pop, verbose = TRUE) {
  if (verbose) print("*** Computing city totals ***")
  df_out <- ccmf$main %>%
    filter(incident_id %in% selected_ids) %>%
    filter(province != "N/A") %>%
    drop_na(city) %>%
    # checkData() %>%
    group_by(province, loc_full) %>%
    summarize(incidents = n()) %>%
    ungroup()
    
  if (nrow(df_out) == 0) {
    if (verbose) {
      print("No city data")
    }
    return(df_out)
  }
  
  df_out %>%
    left_join(ccmf$city_ref %>% select(loc_full, lat, lon),
              by = "loc_full") %>%
    left_join(cities_pop %>% select(loc_full, population, size_group),
              by = "loc_full") %>%
    # drop_na(lon, lat) %>%
    # *** Temporary - drop any rows with missing population ***
    drop_na(lon, lat, population) %>%
    mutate(
      incidents_per100k = 1e5 * incidents / population,
      # Scaled variables to define radius for circles on map -- scales with the 
      # square root of the incidents (or incidents per 100k), so that the area 
      # scales proportionally
      scaled_incidents = sqrt(incidents / max(incidents)),
      scaled_incidents_per100k = sqrt(
        incidents_per100k / max(incidents_per100k, na.rm = TRUE)
      ), 
      label = sprintf(
        "<strong>%s</strong><br>Incidents: %d<br>Incidents per 100k: %.4f", 
        loc_full, 
        incidents,
        incidents_per100k
      ) %>% lapply(htmltools::HTML)
    )
}

dataExplorerDf <- function(ccmf, selected_ids) {
  ccmf$main %>%
    filter(incident_id %in% selected_ids) %>%
    select(incident_date, incident_description, article_url, province,
           city, ethnic_community, ethnic_sub_community,
           identity_based, gender, incident_category, incident_type,
           context) %>%
    arrange(desc(incident_date)) %>%
    rename(
      "Month" = "incident_date",
      "Description" = "incident_description",
      "URL" = "article_url",
      "Province" = "province",
      "Municipality" = "city",
      "Ethnic Community Targeted" = "ethnic_community",
      "Ethnic Sub-Community Targeted" = "ethnic_sub_community",
      "Identity-Based" = "identity_based",
      "Gender of Victim(s)" = "gender",
      "Incident Category" = "incident_category",
      "Incident Type" = "incident_type",
      "Context" = "context"
    ) %>%
    mutate(Month = strftime(Month, format = "%Y-%m"))
}
  