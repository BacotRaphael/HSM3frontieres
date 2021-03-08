# library(tidyverse)
# library(rlang)
# 
# get current group
get_group <- function(df) {
  group <- group_vars(df)
  quo(unique(!!sym(group)))
}

# weighted sum
 sumR <- function(x, df, x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)
  
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    NA
  } else {
    sum(x)
  }
}

# weighted mean
meanR <- function(x, df, digits = 0, x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    NA
  } else {
    round(sum(x) / length(x), digits)
  }
}

# weighted median
medianR <- function(x, df, x_name = NULL, group = NULL, digits =0) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  
  group_var <- group_vars(df)
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    NA
  } else {
    round(median(x), digits)
  }
}


# get percent response
percent_response <- function(x, df, ..., x_name = NULL, group = NULL, digits =0) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)

  args <- list(...)
  args <- unlist(args)
  args <- paste0("\\b", args, "\\b")
  args <- paste0("(", paste0(args, collapse = "|"), ")")
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    NA
  } else {
    pct <- sum(str_detect(x, args)) / length(x)
    round(100 * pct, digits)
  }
}

# get count response
count_response <- function(x, df, ..., x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)
  
  args <- list(...)
  args <- unlist(args)
  args <- paste0("\\b", args, "\\b")
  args <- paste0("(", paste0(args, collapse = "|"), ")")
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    NA
  } else {
    sum(str_detect(x, args))
  }
}

num_percent_response <- function(x, df, ..., group = NULL, digits= 0) {
  x_name <- deparse(substitute(x))
  group_var <- group_vars(df)
  
  args <- list(...)
  args <- unlist(args)
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    NA
  } else {
    pct <- sum((x %in% args)) / length(x)
    round(100 * pct, digits)
  }
}


# get percent of response
select_percents <- function(x, n, df, survey_sheet, choice_sheet, return_what, language = NULL, exclude = NULL, x_name = NULL, group = NULL,digits=0) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  
  group_var <- group_vars(df)
  
  # basic setup
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    if (return_what == "label") {
      NA_character_
    } else {
      NA_integer_
    }
  } else {
    # Getting choices and labels
    l_name <- filter(survey_sheet, name == x_name)$type
    l_name <- str_remove(l_name, "(select_one |select_multiple )")
    choices <- filter(choice_sheet, list_name == l_name)$name
    
    if (!is.null(language)) {
      # cols <- names(choice_sheet)
      # col <- str_detect(cols, paste0("label[\\W]{2}(?i)", language))
      # col <- cols[col]
      col <- language
    } else {
      col <- "label"
    }
    
    labels <- filter(choice_sheet, list_name == l_name)[[col]]
    # finding instances of choice options
    choice_rgx <- str_c("\\b", choices, "\\b")
    counts <- map_dbl(choice_rgx, ~sum(str_count(x, .x)))
    
    if (!is.null(exclude)) {
      choices <- choices[order(counts, decreasing = T)]
      while (choices[n] %in% exclude | is.na(choices[n])) {
        n <- n + 1
      }
    }
    
    if (return_what == "label") {
      labels <- labels[order(counts, decreasing = T)]
      labels[n]
    } else if (return_what == "percent") {
      counts <- counts[order(counts, decreasing = T)]
      round(100 * (counts[n] / length(x)), digits)
    } else if (return_what == "count") {
      counts <- counts[order(counts, decreasing = T)][n]
    }
  }
}

# spread multiple columns

big_spread <- function(df, key, value) {
  # quote key
  keyq <- enquo(key)
  # break value vector into quotes
  valueq <- enquo(value)
  s <- quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

# get names from ellipsis
ellipsis_names <- function(...) {
  args <- as.list(substitute(list(...)))[-1L]
  map_chr(args, deparse)
}

# concatenate columns into string based on value

str_concat <- function(val, ...) {
  col_names <- ellipsis_names(...)
  cols <- list(...)
  cols_match <- map(cols, str_detect, val)
  cols <- map2(cols_match, col_names, ~ ifelse(.x, .y, ""))
  string <- do.call(paste, cols) %>%
    str_remove_all("\\bNA\\b") %>%
    str_squish
  string[string == ""] <- NA
  string
}
