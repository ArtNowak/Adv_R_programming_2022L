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
  } else if ( !(person_classification %in% c('', 'Main', 'Victim', 'Accomplice'))) {
    stop('Invalid value. Valid values: Main, Victim or Accomplice')
  }
  
  if (!is.character(status)) {
    stop('Status must be a string!')
  } else if ( !(person_classification %in% c('', 'na', 'captured', 'recovered',
                                             'located', 'surrendered', 'deceased'))) {
    stop('Invalid value. Valid values: na, captured, recovered, located, surrendered, deceased')
  }
  
  range <- pages/50
  output_df <- data.frame()
  for (i in 1:range) {
    # create the url
    url <- paste0('https://api.fbi.gov/@wanted?pageSize=50', '&page=', i,
                  '&person_classification=', person_classification, '&status=', status)
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
  c('reward_text', 'publication', 'warning_message', 'field_offices', 'sex', 'scars_and_marks', 'build',
    'nationality', 'caution', 'race', 'hair', 'weight', 'place_of_birth', 'status', 'eyes', 'languages',
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



df_new <- df_test[, c('reward_text', 'publication', 'warning_message', 'field_offices', 'sex', 'scars_and_marks', 'build',
                      'nationality', 'caution', 'race', 'hair', 'weight', 'place_of_birth', 'status', 'eyes', 'languages',
                      'possible_countries')]
# reward
df_new$new_reward <- lapply(df_new$reward_text, clean_reward)
# date
df_new <- subset(cbind(df_new, clean_date(df_new$publication)), select = -c(publication))
