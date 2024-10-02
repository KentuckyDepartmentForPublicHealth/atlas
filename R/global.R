


# libs --------------------------------------------------------------------


library(shiny)
library(bslib)
library(dplyr)
library(readr)


# colors ------------------------------------------------------------------

chfs <- list(
  cols2 = c('#95D3F5', '#0C3151'),
  cols3 = c('#62BCF0', '#01203D', '#84BC49'),
  cols4 = c('#5CB2E5', '#0C3151', '#305E4C', '#76AB48'),
  cols5 = c('#95D3F5', '#5CB2E5', '#0C3151', '#517F44', '#9FCA70'),
  cols6 = c('#95D3F5', '#5CB2E5', '#0C3151', '#305E4C', '#517F44', '#9FCA70'),
  cols7 = c('#95D3F5', '#5CB2E5', '#3A7CA6', '#0C3151', '#305E4C', '#517F44', '#9FCA70'),
  cols8 = c('#95D3F5', '#5CB2E5', '#3A7CA6', '#0C3151', '#00060C', '#305E4C', '#517F44', '#9FCA70'),
  cols9 = c('#5CB2E5', '#0C3151', '#305E4C', '#76AB48','#00060C', '#305E4C', '#517F44', '#76AB48', '#9FCA70')
)


# deploy ------------------------------------------------------------------

# currentDate <- format(Sys.time(), '%a, %b %d, %Y at %I:%M %p EDT')
# saveRDS(currentDate, file = 'dat/currentDate.rds')
currentDate <- readRDS(file = 'dat/currentDate.rds')

# rsconnect::deployApp(account = 'kdph', appName = 'atlas', appVisibility = 'private', logLevel = 'verbose')



