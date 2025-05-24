# Note: Not all functions are used in the ETL process
# But are here for interactive development and testing

message("Loading system options, libraries, and functions..")


# system options and libraries --------------------------------------------

# options(dplyr.width = Inf, dplyr.print_max = 1e5, max.print = 1e5, shiny.launch.browser = FALSE)

library(tidyverse)
library(shiny)

# CHFS colors -------------------------------------------------------------
chfs <- list(
  cols2 = c("#95D3F5", "#0C3151"),
  cols3 = c("#62BCF0", "#01203D", "#84BC49"),
  cols4 = c("#5CB2E5", "#0C3151", "#305E4C", "#76AB48"),
  cols5 = c("#95D3F5", "#5CB2E5", "#0C3151", "#517F44", "#9FCA70"),
  cols6 = c("#95D3F5", "#5CB2E5", "#0C3151", "#305E4C", "#517F44", "#9FCA70"),
  cols7 = c("#95D3F5", "#5CB2E5", "#3A7CA6", "#0C3151", "#305E4C", "#517F44", "#9FCA70"),
  cols8 = c("#95D3F5", "#5CB2E5", "#3A7CA6", "#0C3151", "#00060C", "#305E4C", "#517F44", "#9FCA70"),
  cols9 = c("#5CB2E5", "#0C3151", "#305E4C", "#76AB48", "#00060C", "#305E4C", "#517F44", "#76AB48", "#9FCA70")
)


# helper functions --------------------------------------------------------

# short viewer -----
myView <- function(data, sample_size = 5) {
  # Check if sample_size is not larger than the number of rows in the dataset
  if (sample_size > nrow(data)) {
    sample_size <- nrow(data)
    warning("Sample size was larger than data rows; adjusted to data size.")
  }

  # Generate random indices for sampling
  sample_idx <- sample(nrow(data), sample_size)

  # View the sampled data
  View(data[sample_idx, ])
}

# negate in ---------------------------------------------------------------

`%notin%` <- Negate(`%in%`)

# backslash to forward slash ----------------------------------------------

FS <- function() {
  writeClipboard(gsub("\\\\", "/", readClipboard()))
}

# what is loaded in memory ------------------------------------------------

loaded <- function(x, unload = F) {
  libbie <- c("lib", "libs", "libraries", "library", "l")
  dataframie <- c("df", "dfs", "dataframe", "data.frame", "tibble", "data", "d")
  veccie <- c("vec", "vector", "vecs", "vectors", "v")
  funnie <- c("fun", "funs", "f", "function", "functions", "funcs", "func")

  list_of_dataframes <- sort(names(which(unlist(eapply(.GlobalEnv, is.data.frame)))))
  list_of_vectors <- sort(names(which(unlist(eapply(.GlobalEnv, is.vector)))))
  list_of_funs <- sort(names(which(unlist(eapply(.GlobalEnv, is.function)))))

  if (!unload) {
    if (x %in% libbie) {
      . <- list()
      .[[1]] <- sort(library()$results[, 1])
      .[[2]] <- sort(.packages())
      .
    } else if (x %in% dataframie) {
      list_of_dataframes
    } else if (x %in% veccie) {
      list_of_vectors
    } else if (x %in% funnie) {
      list_of_funs
    }
  } else if (unload) {
    if (x %in% libbie) {
      while (length(seq_along(sessionInfo()$otherPkgs)) > 1) {
        detach(paste0("package:", names(sessionInfo()$otherPkgs[1])), character.only = T, force = T)
      }
    } else if (x %in% dataframie) {
      for (df in list_of_dataframes) {
        rm(list = df[1], envir = .GlobalEnv)
      }
    } else if (x %in% veccie) {
      for (vec in list_of_vectors) {
        rm(list = vec[1], envir = .GlobalEnv)
      }
    } else if (x %in% funnie) {
      for (fun in list_of_funs) {
        rm(list = fun[1], envir = .GlobalEnv)
      }
    }
  }
}

# data frame from clipboard -----------------------------------------------

df_from_clip <- function(...) {
  read_delim(clipboard(),
    delim = "\t",
    ...
  )
}


# data frame into clipboard -----------------------------------------------

df_into_clip <- function(df, ...) {
  write.table(
    df,
    "clipboard",
    sep = "\t",
    row.names = F,
    ...
  )
}

copy_df_to_clipboard <- function(df) {
  # Write the data frame to a temporary file
  temp_file <- tempfile(fileext = ".tsv")
  write.table(df, file = temp_file, sep = "\t", row.names = FALSE, quote = FALSE)
  on.exit(unlink(temp_file)) # Clean up the temporary file

  # Use system to execute xclip to copy the file content to clipboard
  system(paste("xclip -selection clipboard -i", temp_file))
  message("Data frame has been copied to the clipboard.")
}



# separate vector with delimiter to clipboard -----------------------------

into_delim <- function(vec, x) {
  if (vec == "char") {
    res1 <- paste0(",", "\'", x, "\'")
    res1[1] <- str_replace(res1[1], ",", "")
    writeClipboard(res1)
  } else if (vec == "num") {
    res2 <- paste0(",", x)
    res2[1] <- str_replace(res2[1], ",", "")
    writeClipboard(res2)
  }
}

# delete column by name or position ---------------------------------------

delete_column <- function(df, i) {
  cols_df <- colnames(df)
  temp <- F
  if (is.character(i)) {
    if (sum(i %in% cols_df) < 1) stop("invalid column name")
    df2 <- df[!names(df) %in% (i)]
    temp <- T
  } else if (is.numeric(i)) {
    if (length(i) > length(cols_df) || max(i) > ncol(df) || i < 1) stop("invalid column index")
    df2 <- df[-(i)]
    temp <- T
  }
  cols_df2 <- colnames(df2)
  amount <- setdiff(cols_df, cols_df2)
  cat("removing", length(amount), "column(s)\n:", amount, "\n")
  return(df2)

  if (!temp) {
    warning("no columns removed")
    return(df)
  }
}

# provide stats about vector ----------------------------------------------

# windows
# vecStats <- function() {
#   x <- readClipboard()
#   x_len <- length(x)
#   if (x_len < 10) {
#     x_char <- nchar(x)
#   } else {
#     x_char <- "Should not compute"
#   }
#   x_distinct <- n_distinct(x)
#   x_sum <- suppressWarnings(sum(as.numeric(x)))

#   if (is.na(x_sum)) {
#     x_sum <- "Unable to compute"
#   }
#   cat(
#     "\n Length = ", x_len, "\n",
#     "Characters = ", x_char, "\n",
#     "Distinct = ", x_distinct, "\n",
#     "Sum = ", x_sum, "\n"
#   )
# }

# unix
vecStats <- function() {
  # Get clipboard content using xclip on Linux
  x <- system("xclip -o -selection clipboard", intern = TRUE)

  # Get the length of the clipboard content
  x_len <- length(x)

  # Check if the length is less than 10 to compute the character count
  if (x_len < 10) {
    x_char <- nchar(x)
  } else {
    x_char <- "Should not compute"
  }

  # Get the number of distinct elements
  x_distinct <- length(unique(x))

  # Try to compute the sum of the numeric values
  x_sum <- suppressWarnings(sum(as.numeric(x)))

  # Handle NA values in sum
  if (is.na(x_sum)) {
    x_sum <- "Unable to compute"
  }

  # Print the results
  cat(
    "\n Length = ", x_len, "\n",
    "Characters = ", x_char, "\n",
    "Distinct = ", x_distinct, "\n",
    "Sum = ", x_sum, "\n"
  )
}
# rename all columns; preserve old names ----------------------------------

rename_all_columns <- function(df) {
  old_col_names <<- names(df)
  df %>%
    rename_all(~ str_c("col", seq_along(.)))
}

# classify an object ------------------------------------------------------

huh <- function(x) {
  cat("typeof = ", typeof(x))
  cat("\nclass = ", class(x))
  cat("\nstorage mode = ", storage.mode(x))
  cat("\nobject type = ", sloop::otype(x))
  cat("\nobject size = ", lobstr::obj_size(x))
  cat("\nobject size (Kb) = ", object.size(x) %>% format("Kb"))
  cat("\nobject size (Mb) = ", object.size(x) %>% format("Mb"))
  cat("\nobject size (Gb) = ", object.size(x) %>% format("Gb"))
  cat("\nast = ", try(lobstr::ast(x)))
  cat("\nlength = ", length(x))
}

# contigency table with stats ---------------------------------------------

freq <- function(x) {
  y <- table(x, useNA = "always")
  z <- as_tibble(y, .name_repair = ~ c("var", "count"))
  z %>%
    mutate(
      count_all = sum(count),
      fraction = count / count_all
    ) %>%
    arrange(desc(count)) %>%
    mutate(
      fraction_rolling = cumsum(fraction),
      count_rolling = cumsum(count)
    ) %>%
    select(var, count, count_rolling, count_all, fraction, fraction_rolling)
}

## wrapper
f <- function() {
  freq(readClipboard())
}

## convenience
# r <- function() { readClipboard() }

# percent change ----------------------------------------------------------

deltaP <- function(x, y, as_decimal = F) {
  z <- (y - x) / x
  if (!as_decimal) {
    scales::percent(z, 0.01)
  } else {
    z
  }
}

# either this or that -----------------------------------------------------


# `%||%` <- function(a, b) {
#   ifelse(!is.na(a), a, b)
# }

# create a data frame with interval dates ---------------------------------

createDt <- function(from, to, by) {
  by <- tolower(by)
  # creates a i x 7 tibble, from a to b by c
  stopifnot(
    "invalid data types; or missing argument(s)" =
      is.Date(from) && is.Date(to) && length(by) == 1
  )

  x <- seq.Date(
    from = from,
    to = to,
    by = by
  )

  tibble(
    dt_period = x
  ) %>%
    mutate(
      dt_seq_id = row_number(),
      dt_seq_lag = dt_seq_id - 1,
      dt_start_orig = from,
      dt_end_orig = to,
      dt_range_requested = by,
      dt_range_elapsed = paste0(dt_seq_id, " ", "(", dt_range_requested, ")"),
      dt_range_elapsed_lag = paste0(dt_seq_lag, " ", "(", dt_range_requested, ")")
    ) %>%
    select(
      dt_seq_id,
      dt_seq_lag,
      dt_period,
      dt_range_requested,
      dt_range_elapsed,
      dt_range_elapsed_lag,
      everything()
    )
} # end func definition


# show column names -------------------------------------------------------

# instead of sort(names(df))

sCols <- function(x, sorted = T, howMuch = 1:ncol(x)) {
  a <- names(x)
  a <- a[howMuch]

  b <- sort(names(x))
  b <- b[howMuch]

  if (sorted) {
    cat(paste(1:length(b), b, collapse = "\n"))
  } else {
    cat(paste(1:length(a), a, collapse = "\n"))
  }
}


# length intersect --------------------------------------------------------

li <- function(a, b) {
  lenA <- length(a)
  lenB <- length(b)
  d <- length(intersect(a, b))

  e <- which.min(c(lenA, lenB))
  f <- c(lenA, lenB)
  cat("Found", d, "of", f[e], "instance(s) of intersection")
}



# p2o, o2p ----------------------------------------------------------------

p2o <- function(p) {
  p / (1 - p)
}
o2p <- function(o) {
  o / (1 + o)
}



# all caps and prefix -----------------------------------------------------

allCaps <- function(df, x) {
  df %>%
    mutate(
      across(
        .cols = everything(),
        str_to_upper
      )
    )
}

addPrefix <- function(df, prefix) {
  df %>%
    rename_with(
      ~ paste0(prefix, "_", .)
    )
}


# list funs ---------------------------------------------------------------

listFuns <- function(pkg) {
  string <- paste0("package:", pkg)
  lsf.str(string)
}

lsf <- function(pkg) {
  listFuns(pkg)
}


# history maker -----
h <- function(n = 5) {
  # Read the history file
  hist_file <- "~/.radian_history"
  if (!file.exists(hist_file)) {
    hist_file <- "~/.local/share/radian/history"
  }

  hist_lines <- readLines(hist_file)

  # Initialize variables to collect commands
  commands <- list()
  current_command <- NULL
  timestamp <- NULL

  # Process the history lines
  for (line in hist_lines) {
    if (grepl("^# time:", line)) {
      # If we have a command in progress, save it
      if (!is.null(current_command)) {
        commands <- c(commands, list(list(
          time = timestamp,
          code = paste(current_command, collapse = "\n")
        )))
      }

      # Start a new command
      timestamp <- gsub("^# time: (.*) UTC", "\\1", line)
      current_command <- NULL
    } else if (grepl("^# mode:", line)) {
      # Skip mode lines
      next
    } else if (!is.null(timestamp)) {
      # Add to current command, removing leading '+' if present
      line <- gsub("^\\+", "", line)
      current_command <- c(current_command, line)
    }
  }

  # Add the last command if there is one
  if (!is.null(current_command)) {
    commands <- c(commands, list(list(
      time = timestamp,
      code = paste(current_command, collapse = "\n")
    )))
  }

  # Get the last n commands
  last_commands <- tail(commands, n)

  # Display them nicely
  cat("\n")
  for (i in seq_along(last_commands)) {
    cmd <- last_commands[[i]]
    cat("─────────────────────────────────────────\n")
    cat("Command", i, "at", cmd$time, "\n")
    cat("─────────────────────────────────────────\n")
    cat(cmd$code, "\n\n")
  }
}

# stopper -----------------------------------------------------------------


message("System ready.")
