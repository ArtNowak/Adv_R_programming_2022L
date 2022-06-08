library(httr)
library(jsonlite)
library(fastDummies)
library(tidyr)

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
# SUPER DUPE £UKASZ PART
df_new$`Flight Risk` <- ifelse(df_new$`Flight Risk` == 'TRUE', 1, 0)
df_new$Dangerous <- ifelse(df_new$Dangerous == 'TRUE', 1, 0)
df_new$Armed <- ifelse(df_new$Armed == 'TRUE', 1, 0)
df_new$`Escape Risk` <- ifelse(df_new$`Escape Risk` == 'TRUE', 1, 0)
df_new$`Extremely Dangerous` <- ifelse(df_new$`Extremely Dangerous` == 'TRUE', 1, 0)
df_new$`High-Risk Arrest` <- ifelse(df_new$`High-Risk Arrest` == 'TRUE', 1, 0)
df_new$`Violent Tendencies` <- ifelse(df_new$`Violent Tendencies` == 'TRUE', 1, 0)
df_new$`May Abuse Drugs` <- ifelse(df_new$`May Abuse Drugs` == 'TRUE', 1, 0)
df_new$`Suicidal Tendencies` <- ifelse(df_new$`Suicidal Tendencies` == 'TRUE', 1, 0)
df_new$`Bi-Polar` <- ifelse(df_new$`Bi-Polar` == 'TRUE', 1, 0)
df_new$`In Need Of Medications` <- ifelse(df_new$`In Need Of Medications` == 'TRUE', 1, 0)
df_new$`Suicide Risk` <- ifelse(df_new$`Suicide Risk` == 'TRUE', 1, 0)
df_new$sex <- ifelse(df_new$sex == 'Female', 1, 0)

df_new <- dummy_cols(df_new, select_columns = c("race", "hair", "eyes"))
df_new <- subset(df_new, select = -c(build, nationality, race, hair, eyes, status, possible_countries, nationality,
                                     hair_NA, eyes_NA, race_NA))

states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware",
            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
            "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
            "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
            "West Virginia", "Wisconsin", "Wyoming")

df_new$place_of_birth <- ifelse(sub(".*, ", "", df_new$place_of_birth) %in% states, 1,
                                ifelse(is.na(df_new$place_of_birth), NA, 0))

##LISTA ¯ALI
#status/possible_countries/build dropuje, za du¿o na
#nationality imho bez sensu, bo to FBI wiêc bêdzie du¿o hamerykanów i biased set

# scars_and_marks
df_new$scars_and_marks <- ifelse(!is.na(df_new$scars_and_marks), 1, 0)
#weight
df_new$weight <- replace(df_new$weight, df_new$weight == "Unknown", NA)
df_new$weight <- replace(df_new$weight, df_new$weight == "", NA)
df_new$weight <- replace(df_new$weight, df_new$weight == "Unknown (heavy-set build)", NA)
df_new$weight <- replace(df_new$weight, df_new$weight == "Infant (at the time of disappearance)", NA)


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


#place of birth
df_new$place_of_birth <- sapply(strsplit(df_new[['place_of_birth']], ', '), tail, 1)
