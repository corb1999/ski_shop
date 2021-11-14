# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-11-14"), 
                                    author = "corb", 
                                    proj_name = "ski_shop", 
                                    script_type = "crawler", 
                                    notepad = paste0("rvest to webcrawl")), 
                  seed_set = 6)
metadatar

# LOAD LIBRARIES **********************************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
# library(gt)
library(rvest)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5)
mem_used()

# basic helper functions ***************************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  return(dd)}

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# POST SCRIPT; alt to using paste0() all the time (i saw this on twitter)
'%ps%' <- function(lhs, rhs) {
  return_me <- paste0(lhs, rhs)
  return(return_me)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# set urls ---------------------------------------------------------

# confirmed in robots.txt file that it is ok to crawl this base url
crawl_urls <- list(url_base = 'https://www.evo.com/shop', 
                   url_suffix = c('/sale/ski/mens/shipsto_us/online'), 
                   url_pages = paste0('p_', seq(1, 10)))

crawl_urls

paste0(crawl_urls$url_base, crawl_urls$url_suffix, crawl_urls$url_pages[1])

# ^ -----

# crawling action -----------------------------------------

asdf <- read_html(paste0(crawl_urls$url_base, 
                         crawl_urls$url_suffix, 
                         crawl_urls$url_pages[1]))


# asdf %>% html_elements('.results-count') %>% html_text2()
# 
# aa <- asdf %>% html_elements('.product-thumb-details') 
# 
# aa %>% html_elements('.product-thumb-title') %>% length()
# 
# asdf %>% html_elements('.product-thumb-title') %>% html_text2()
# asdf %>% html_elements('.product-thumb-price') %>% html_text2()
# asdf %>% html_elements('.product-thumb-sale') %>% html_text2()
# asdf %>% html_elements('.discount') %>% html_text2()


raw_df <- data.frame(product = asdf %>% 
                       html_elements('.product-thumb-title') %>% 
                       html_text2())
raw_df <- cbind(raw_df, data.frame(rid = c(seq(1, nrow(raw_df)))))
raw_df

interim_product <- rbind(raw_df, raw_df) %>% arrange(rid) %>% 
  select(-rid)

raw_prices <- data.frame(all_prices = asdf %>% 
                           html_elements('.product-thumb-price') %>% 
                           html_text2(), 
                         product = interim_product) %>% 
  mutate(interim = row_number(), 
         interim = ifelse(interim %% 2 == 0, 
                          'discount_price', 'regular_price')) %>% 
  pivot_wider(names_from = interim, values_from = all_prices)
raw_prices %>% View()


# ^ -----



