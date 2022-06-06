library(httr)
library(jsonlite)

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
  c('reward_text', 'publication', 'warning_message', 'field_offices', 'sex',
    'scars_and_marks', 'build', 'nationality', 'caution', 'race', 'hair',
    'weight', 'place_of_birth', 'status', 'eyes',
    'possible_countries')
  
  input_df$reward <- parse_number(input_df[, 'reward_text'])
}


df_test <- get_api_data(1000)

library(stringr)

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
  
  year <- format(as.Date(date_col), '%Y')
  month <- format(as.Date(date_col), '%m')
  day <- format(as.Date(date_col), '%d')
  
  output_df <- data.frame(Year = year, Month = month, Day = day)
  return(output_df)
}


# split list variables
library(dplyr)
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
library(stringr)
clear_warning_message <- function(df) {
  possible_threats <- c('FLIGHT RISK', 'DANGEROUS', 'ARMED', 'ESCAPE RISK',
                        'EXTREMELY DANGEROUS', 'HIGH-RISK ARREST', 
                        'VIOLENT TENDENCIES', 'MAY ABUSE DRUGS',
                        'SUICIDAL TENDENCIES', 'BI-POLAR',
                        'IN NEED OF MEDICATIONS', 'SUICIDE RISK')
  for (i in possible_threats) {
    df[[str_to_title(i)]] <- ifelse(grepl(i, df$warning_message), T, F)
  }
  df <- within(df, rm('warning_message'))
  return(df)
}


df_new <- df_test[, c('reward_text', 'publication', 'warning_message',
                      'field_offices', 'sex', 'scars_and_marks', 'build',
                      'nationality', 'race', 'hair', 'weight', 'place_of_birth',
                      'status', 'eyes', 'possible_countries',
                      'height_max', 'height_min')]

#height
df_new$height <- lapply((df_new$height_max + df_new$height_min)/2, as.numeric)
df_new <- subset(df_new, select = -c(height_max, height_min))
# reward
df_new$new_reward <- lapply(df_new$reward_text, clean_reward)
df_new <- subset(df_new, select = -c(reward_text))
# date
df_new <- subset(cbind(df_new, clean_date(df_new$publication)),
                 select = -c(publication))
# vector cols
df_new <- split_vector_cols(c('field_offices', 'possible_countries'), df_new)
# warning_message
df_new <- clear_warning_message(df_new)
# todo - dummies

# scars_and_marks
df_new$scars_and_marks <- ifelse(!is.na(df_new$scars_and_marks), T, F)
#weight
# to do - naprawic xD bo sa rozne wartosci, wez jakis parse_number czy cos
# i jak sa 2 wartosci to srednia wyciagnij idk
df_new$weight <- sub(" pounds.*", "", df_new$weight)

#place of birth
df_new$place_of_birth <- sapply(strsplit(df_new[['place_of_birth']], ', '), tail, 1)
