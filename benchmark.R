test_df <- fbi_df

# 1
lapply(reward_text, clean_reward)

new_list <- c()
for (i in df$reward_text) {
  append(new_list, clean_reward(i))
}

#2
binary_vars <- sapply(cbind(X_train, y_train),
                      function(x) { all(x %in% 0:1) })

binary_list <- c()
for (i in colnames(clean_fbi_df)) {
  if (all(df[[i]] %in% 0:1)) {
    append(binary_list, i)
  }
}


# 3
test_df <- test_df %>%
  replace(length(.) == 0, NA)

# spread values into rows
for (i in c('field_offices')) {
  test_df <- test_df %>%
    unnest_wider(col = all_of(i), names_sep = '_')
}


colnames(map_dfc(c('field_offices'), 
        ~ unnest_wider(test_df[.x], all_of(.x), names_sep = "_")))
