Sys.setlocale("LC_TIME", "English")
library(httr)
library(jsonlite)
library(fastDummies)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)

get_api_data <- function(pages, person_classification = '', status = '') {
  if (!is.numeric(pages)){
    stop('Pages is not a number!')
  } else if (pages %% 50 != 0) {
    stop('Pages is not dividable by 50!')
  } 

  if (!is.character(person_classification)) {
    stop('Person_classification must be a string!')
  } else if ( !(person_classification %in% c('', 'Main', 'Victim',
                                             'Accomplice'))) {
    stop('Invalid value. Valid values: Main, Victim or Accomplice')
  }
  
  if (!is.character(status)) {
    stop('Status must be a string!')
  } else if ( !(person_classification %in% c('', 'na', 'captured',
                                             'recovered', 'located',
                                             'surrendered', 'deceased'))) {
    stop('Invalid value. Valid values: na, captured, recovered, located,
         surrendered, deceased')
  }
  
  range <- pages/50
  output_df <- data.frame()
  for (i in 1:range) {
    # create the url
    url <- paste0('https://api.fbi.gov/@wanted?pageSize=50', '&page=', i,
                  '&person_classification=', person_classification,
                  '&status=', status)
    # get the response and convert to json
    response <- GET(url)
    data <- content(response, as = 'text')
    new <- fromJSON(data)
    # extract only df with meaningful data
    df <- new$items
    #bind to the output df
    output_df <- rbind(output_df, df)
  }
  
  return(output_df)
}


clean_api_data <- function(input_df) {
  desired_cols <- c('reward_text', 'publication', 'warning_message',
                    'field_offices', 'sex', 'scars_and_marks', 'build', 
                    'nationality', 'race', 'hair', 'weight', 'place_of_birth',
                    'status', 'eyes', 'possible_countries', 'height_min',
                    'height_max')
  
  df_new <- input_df[, desired_cols]
  
  # tego nie robimy
  #input_df$reward <- parse_number(input_df[, 'reward_text'])
  
  # reward
  clean_reward <- function(reward_row) {
    if (is.na(reward_row)) {
      return('No information')
    } else {
      first_split <- strsplit(reward_row, 'up to ')[[1]][2]
      if(is.na(first_split)) {
        return('No specified amount')
      } else {
        number_with_dollar <- strsplit(first_split, ' ')[[1]][c(1, 2)]
        # find million in description
        if (number_with_dollar[2] == 'million') {
          #remove $ from the beginning and , as a separator, convert to numeric
          return(as.numeric(gsub('^.|,', '', number_with_dollar[1])) * 1000000)
        } else {
          return(as.numeric(gsub('^.|,', '', number_with_dollar[1])))
        }
      }
    }
  }
  
  # date
  clean_date <- function(date_col) {
    date_test <- try(as.Date(date_col))
    if ('try-error' %in% class(date_test) || is.na(date_test)) {
      stop('Not a date column')
    }
    
    year <- as.numeric(format(as.Date(date_col), '%Y'))
    month <- as.numeric(format(as.Date(date_col), '%m'))
    day <- as.numeric(format(as.Date(date_col), '%d'))
    
    weekday <- wday(as.Date(date_col), label=TRUE)
    
    output_df <- data.frame(Year = year, Month = month,
                            Day = day, Weekday = weekday)
    
    for (i in unique(output_df$Weekday)) {
      output_df[[str_to_title(i)]] <- ifelse(grepl(i, output_df$Weekday), 1, 0)
    }
    
    output_df <- within(output_df, rm('Weekday'))
    return(output_df)
  }
  
  # split list variables
  split_vector_cols <- function(col_names, df) {
    for (i in col_names) {
      #deal with empty lists
      df[[i]][lapply(df[[i]], length) == 0] <- NA
    }
    for (i in col_names) {
      # spread values into rows
      df <- unnest_wider(df, col = i, names_sep = '_')
    }
    df <- df
    return (df)
  }
  
  # warning_messages
  clear_warning_message <- function(df) {
    possible_threats <- c('FLIGHT RISK', 'DANGEROUS', 'ARMED', 'ESCAPE RISK',
                          'EXTREMELY DANGEROUS', 'HIGH-RISK ARREST', 
                          'VIOLENT TENDENCIES', 'MAY ABUSE DRUGS',
                          'SUICIDAL TENDENCIES', 'BI-POLAR',
                          'IN NEED OF MEDICATIONS', 'SUICIDE RISK')
    for (i in possible_threats) {
      df[[str_to_title(i)]] <- ifelse(grepl(i, df$warning_message), 1, 0)
    }
    df <- within(df, rm('warning_message'))
    return(df)
  }
  
  # field offices dummies
  create_off_dummies <- function(df) {
    offices <- c('field_offices_1', 'field_offices_2', 'field_offices_3')
    offices_dummies <- data.frame()
    for (office in offices) {
      if (office == offices[1]) {
        offices_df <- dummy_cols(df[, office], select_columns = office,
                                 remove_selected_columns = T) %>% 
          rename_all(~ gsub(paste0(office, '_'), "", .x)) %>%
          replace(is.na(.), 0)
      } else {
        dummies_df <- dummy_cols(df[, office], select_columns = office,
                                 remove_selected_columns = T) %>% 
          rename_all(~ gsub(paste0(office, '_'), "", .x)) %>%
          replace(is.na(.), 0)
        for (colname in colnames(dummies_df)) {
          offices_df[[colname]] == offices_df[[colname]] + dummies_df[[colname]]
        }
      }
    }
    output_df <- cbind(df, offices_df)
    output_df <- output_df[, !colnames(output_df) %in% offices]
    return(output_df)
  }
  
  #h eight
  df_new$height <- as.numeric((df_new$height_max + df_new$height_min)/2)
  df_new <- subset(df_new, select = -c(height_max, height_min))
  
  # reward
  df_new$new_reward <- lapply(df_new$reward_text, clean_reward)
  df_new <- subset(df_new, select = -c(reward_text))
  # date
  df_new <- subset(cbind(df_new, clean_date(df_new$publication)),
                   select = -c(publication))
  # vector cols
  df_new <- split_vector_cols(c('field_offices'), df_new)
  
  # warning_message
  df_new <- clear_warning_message(df_new)
  
  # sex
  df_new$sex <- ifelse(df_new$sex == 'Female', 1, 0)
  
  # race, hair, eyes dummies
  df_new <- dummy_cols(df_new, select_columns = c("race", "hair", "eyes"))
  df_new <- subset(df_new, select = -c(build, nationality, race, hair, eyes, status, possible_countries, nationality,
                                       hair_NA, eyes_NA, race_NA))
  
  # place of birth (USA)
  states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware",
              "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
              "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
              "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
              "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
              "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
              "West Virginia", "Wisconsin", "Wyoming")
  
  df_new$born_in_usa <- ifelse(sub(".*, ", "", df_new$place_of_birth) %in% states, 1,
                               ifelse(is.na(df_new$place_of_birth), NA, 0))
  df_new <- within(df_new, rm('place_of_birth'))
  
  # scars_and_marks
  df_new$scars_and_marks <- ifelse(!is.na(df_new$scars_and_marks), 1, 0)
  
  #weight
  df_new$weight <- replace(df_new$weight, df_new$weight == "Unknown", NA)
  df_new$weight <- replace(df_new$weight, df_new$weight == "", NA)
  df_new$weight <- replace(df_new$weight, df_new$weight == "Unknown (heavy-set build)", NA)
  df_new$weight <- replace(df_new$weight, df_new$weight == "Infant (at the time of disappearance)", NA)
  
  # weight loop
  for (i in 1:length(df_new$weight)){
    while (is.na(df_new$weight[i])){
      i <- i + 1
    }
    gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))
    temp <- as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i])))
    if (floor(log10(temp)) == 1){
      temp <- gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))
      df_new$weight[i] <- temp
    }else if (floor(log10(temp))  == 2){
      temp <- gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))
      df_new$weight[i] <- temp
    }else if (floor(log10(temp))  == 3){
      temp <- (as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))),1,2)) + 
                 as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))),3,4)))/2
      df_new$weight[i] <- temp
    }else if (floor(log10(temp))  == 4){
      temp <- (as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))),1,2)) + 
                 as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))),3,5)))/2
      df_new$weight[i] <- temp
    }else if (floor(log10(temp))  == 5){
      temp <- (as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))),1,3)) + 
                 as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", df_new$weight[i]))),4,6)))/2
      df_new$weight[i] <- temp
    }
  }
  df_new$weight <- as.numeric(df_new$weight)
  
  final_df <- create_off_dummies(df_new)
  final_df <- na.omit(final_df)
  
  return(final_df)
}


fbi_df <- get_api_data(1000)
clean_fbi_df <- clean_api_data(fbi_df)
# zostawilem reward, ale to usune w modelowaniu - fajnie jakby to bylo na plocie - jakies biny czy inne cuda 
glimpse(clean_fbi_df)
