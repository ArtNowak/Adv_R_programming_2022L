Sys.setlocale("LC_TIME", "English")
library(httr)
library(jsonlite)
library(fastDummies)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(SHAPforxgboost)
library(lime)
library(caret)
library(xgboost)
library(purrr)


get_api_data <- function(pages, person_classification = '', status = '') {
  if (!is.numeric(pages)){
    stop('Pages is not a number!')
  } else if (pages %% 50 != 0) {
    stop('Pages is not dividable by 50!')
  } else if (!is.character(person_classification)) {
    stop('Person_classification must be a string!')
  } else if ( !(person_classification %in% c('', 'Main', 'Victim',
                                             'Accomplice'))) {
    stop('Invalid value. Valid values: Main, Victim or Accomplice')
  } else if (!is.character(status)) {
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

clean_api_data <- function(input_df, rm_reward = T) {
  if (!is.data.frame(input_df)){
    stop('First argument must be a DataFrame!')
  } else if (!is.logical(rm_reward)) {
    stop('Second argument must be a boolean!')
  }

  desired_cols <- c('reward_text', 'publication', 'warning_message',
                    'field_offices', 'sex', 'scars_and_marks', 'build', 
                    'nationality', 'race', 'hair', 'weight', 'place_of_birth',
                    'status', 'eyes', 'possible_countries', 'height_min',
                    'height_max')
  
  df_new <- input_df[, desired_cols]
  
  # reward
  clean_reward <- function(reward_row) {
    if (is.na(reward_row)) {
      return('No information')
    }
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
  
  # date
  clean_date <- function(date_col) {
    date_test <- try(as.Date(date_col))
    if ('try-error' %in% class(date_test) || is.na(date_test)) {
      stop('Not a date column')
    }
    
    weekday <- wday(as.Date(date_col), label=TRUE)
    
    output_df <- data.frame(Weekday = weekday)
    
    for (i in unique(output_df$Weekday)) {
      output_df[[str_to_title(i)]] <- ifelse(grepl(i, output_df$Weekday), 1, 0)
    }
    
    output_df <- within(output_df, rm('Weekday'))
    return(output_df)
  }
  
  # split list variables
  split_vector_cols <- function(df, col_names) {
    if (!is.vector(col_names)) {
      stop('Second argument must be a vector!')
    } else if (!is.data.frame(df)) {
      stop('First argument must be a DataFrame!')
    }
    # replace empty lists with NA
    df[col_names] <- df[col_names] %>%
      replace(length(.) == 0, NA)
    
    field_offices <- map_dfc(col_names, 
                             ~ unnest_wider(df[.x], all_of(.x), names_sep = "_"))
    
    offices <- colnames(field_offices)
    
    for (office in offices) {
      if (office == offices[1]) {
        offices_df <- dummy_cols(field_offices[office], select = office,
                                 remove_selected_columns = T) %>% 
          rename_all(~ gsub(paste0(office, '_'), "", .x)) %>%
          replace(is.na(.), 0)
      } else {
        dummies_df <- dummy_cols(field_offices[office], select = office,
                                 remove_selected_columns = T) %>% 
          rename_all(~ gsub(paste0(office, '_'), "", .x)) %>%
          replace(is.na(.), 0)
        for (colname in colnames(dummies_df)) {
          offices_df[[colname]] == offices_df[[colname]] + dummies_df[[colname]]
        }
      }
    }
    output_df <- cbind(df, offices_df)
    output_df <- output_df[, !colnames(output_df) %in% c(offices, 'NA')]
    
    return (output_df)
  }
  
  # warning_messages
  clear_warning_message <- function(df) {
    if (!is.data.frame(df)) {
      stop('Argument must be a DataFrame!')
    }
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

  #weight
  transform_weight <- function(weight_col) {
    weight_col[weight_col %in% c("Unknown", "", "Unknown (heavy-set build)",
                                 "Infant (at the time of disappearance)")] <- NA

    column_transformer <- function(col) {
      for (i in 1:length(col)) {
        while (is.na(col[i])) {
          i <- i + 1
        }
        gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))
        temp <- as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i])))
        if (floor(log10(temp)) == 1){
          temp <- gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))
          col[i] <- temp
        }else if (floor(log10(temp))  == 2){
          temp <- gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))
          col[i] <- temp
        }else if (floor(log10(temp))  == 3){
          temp <- (as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))),1,2)) +
                     as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))),3,4)))/2
          col[i] <- temp
        }else if (floor(log10(temp))  == 4){
          temp <- (as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))),1,2)) +
                     as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))),3,5)))/2
          col[i] <- temp
        }else if (floor(log10(temp))  == 5){
          temp <- (as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))),1,3)) +
                     as.numeric(substr(as.numeric(gsub("\\D+", "", gsub("\\s*\\([^\\)]+\\)", "", col[i]))),4,6)))/2
          col[i] <- temp
        }
      }
    }
    
    weight_col <- weight_col %>%
      column_transformer
    
    return(weight_col)
  }
  
  # place of birth (USA)
  states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware",
              "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
              "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
              "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
              "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
              "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
              "West Virginia", "Wisconsin", "Wyoming")
  
  final_df <- df_new %>% 
    split_vector_cols(col_names = c('field_offices')) %>%
    clear_warning_message() %>%
    cbind(clean_date(df_new$publication)) %>%
    mutate(height = (height_max + height_min)/2,
           new_reward = lapply(reward_text, clean_reward),
           sex = ifelse(sex == 'Female', 1, 0),
           born_in_usa = ifelse(sub(".*, ", "", place_of_birth) %in% states, 1,
                                ifelse(is.na(place_of_birth), NA, 0)),
           weight = transform_weight(weight),
           scars_and_marks = ifelse(!is.na(df_new$scars_and_marks), 1, 0)) %>%
    dummy_cols(select_columns = c('race', 'hair', 'eyes')) %>%
    subset(select = -c(build, nationality, race, race_, hair, hair_, eyes, eyes_, status,
                       possible_countries, nationality, hair_NA, eyes_NA, race_NA,
                       height_max, height_min, reward_text, publication, place_of_birth, field_offices)) %>%
    na.omit()
  
  if (rm_reward) {
    final_df <- final_df %>%
      subset(select = -c(new_reward))
  }
  
  return(final_df)
}

fbi_df <- get_api_data(1000)
clean_fbi_df <- clean_api_data(fbi_df)

glimpse(clean_fbi_df)

plotting_graphs <- function(data, column){
  if (!is.data.frame(data)){
    stop('First argument must be a DataFrame!')
  } else if (!is.character(column)) {
    stop("Second argument can only take values: 'sex', 'scars_and_marks', 'Dangerous', 'born_in_usa'")
  }
  
  cols_list <- c("sex", "scars_and_marks", "Dangerous", "born_in_usa")
  
  if (any(cols_list == column)){
    if (column == "sex"){
      temp <- table(data[, column])
      barplot(temp, main = "Gender breakdown of wanted persons",
              xlab = "Male/Female", col = c("darkblue", "red"))
    }else if (column == "scars_and_marks"){
      temp <- table(data[, column])
      barplot(temp, main = "Does wanted person have scars/marks",
              xlab = "No/Yes", col = c("darkblue", "red"))
    }else if (column == "Dangerous"){
      temp <- table(data[, column])
      barplot(temp, main = "Is wanted person dangerous?",
              xlab = "No/Yes", col = c("darkblue", "red"))
    }else if (column == "born_in_usa"){
      temp <- table(data[, column])
      barplot(temp, main = "Was wanted person born in USA?",
              xlab = "No/Yes", col = c("darkblue", "red"))
    }
  }else {
    print("Only columns: 'sex', 'scars_and_marks', 'Dangerous', 'born_in_usa' can plotted!")
  }
}

plotting_graphs(clean_fbi_df, "sex")
plotting_graphs(clean_fbi_df, "scars_and_marks")
plotting_graphs(clean_fbi_df, "Dangerous")
plotting_graphs(clean_fbi_df, "born_in_usa")
plotting_graphs(clean_fbi_df, "Age")

train_test_split <- function(input_df, train_size = .75, dependent_variable) {
  if (!is.data.frame(input_df)){
    stop('First argument must be a DataFrame!')
  } else if (!is.numeric(train_size)) {
    stop('Second argument must be numeric!')
  } else if (train_size >= 1 | train_size <= 0) {
    stop('Second argument must take values from range (0, 1)!')
  } else if (!is.character(dependent_variable)) {
    stop('Third arguement must be a character!')
  } else if (!dependent_variable %in% colnames(input_df)) {
    stop('Third argument must be a column name. Try calling colnames() on your df.')
  }
  
  sample <- sample.int(n = nrow(input_df), size = floor(train_size * nrow(input_df)), replace = F)
  train <- input_df[sample, ]
  test <- input_df[-sample, ]
  
  y_train <- train[dependent_variable]
  y_test <- test[dependent_variable]
  X_train <- train[, !(names(train) %in% dependent_variable)]
  X_test <- test[, !(names(test) %in% dependent_variable)]
  
  return(list(X_train, X_test, y_train, y_test))
}

set.seed(2137)
train_test_list <- train_test_split(clean_fbi_df, train_size = 0.75, dependent_variable = 'Flight Risk')

X_train <- train_test_list[[1]]
X_test <- train_test_list[[2]]
y_train <- train_test_list[[3]]
y_test <- train_test_list[[4]]


train_xgboost <- function(X_train, X_test, y_train, y_test) {
  if (!is.data.frame(X_train) | !is.data.frame(X_test) |
      !is.data.frame(y_train) | !is.data.frame(y_test)) {
    stop('Every argument must be a DataFrame!')
  } 
  # binary variable
  binary_vars <- sapply(cbind(X_train, y_train),
                        function(x) { all(x %in% 0:1) })
  # retrieve dependent variable from y df
  dependent_variable <- colnames(y_train)
  
  if (dependent_variable %in% names(binary_vars)) {
    xgb <- xgboost(data = data.matrix(X_train),
                   label = y_train[[dependent_variable]],
                   nrounds = 25,
                   objective = "binary:logistic",
                   eval_metric = "auc")
    y_pred <- predict(xgb, data.matrix(X_test))
    prediction <- as.numeric(y_pred > 0.5)
  } else {
    xgb <- xgboost(data = data.matrix(X_train),
                   label = y_train[[dependent_variable]],
                   nrounds = 25,
                   eval_metric = "rmse")
    y_pred <- predict(xgb, data.matrix(X_test))
  }
  return(xgb)
}

xgb_model <- train_xgboost(X_train, X_test, y_train, y_test)

setOldClass('xgb.Booster') # because xgb is S3 and we want to obtain clear code
explainer <- setClass("explainer",
                      slots = c(model = "xgb.Booster",
                                X_train = "data.frame",
                                X_test = "data.frame"),
                      )

setGeneric(name = "local_explain",
           def = function(x, y) {
             standardGeneric("local_explain")
           })

setMethod("local_explain",
          signature = c("explainer", "numeric"),
          definition = function(x, y) {
            range <- nrow(x@X_test)
            if (y < 1 | y > range) {
              stop(paste('Your y must be in range 1 :', range))
            }
            explainer <- lime(x@X_train, x@model)
            explanation <- explain(x@X_test[y, ], explainer,
                                   labels = 1, n_features = 5)
            plot_features(explanation)
          } 
)

setGeneric(name = "global_explain",
           def = function(x, y) {
             standardGeneric("global_explain")
           })

setMethod("global_explain",
          signature = c("explainer", "numeric"),
          definition = function(x, y) {
            range <- length(colnames((x@X_test)))
            if (y < 1 | y > range) {
              stop(paste('Your y must be in range 1 :', range, 'although max 15 is recommended'))
            } else if (y > 15) {
              warning("Max 15 features are recommended due to chart's readability")
            }
            shap_values <- shap.values(xgb_model = x@model,
                                       X_train = as.matrix(x@X_train))
            shap_long <- shap.prep(shap_contrib = shap_values$shap_score,
                                   X_train = as.matrix(X_train),
                                   top_n = y)
            shap.plot.summary(shap_long)
          } 
)

expl <- explainer(model = xgb_model,
                  X_train = X_train,
                  X_test = X_test)

local_explain(expl, 84)
global_explain(expl, 15)
