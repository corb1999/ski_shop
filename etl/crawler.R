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
crawl_urls <- data.frame(url_base = 'https://www.evo.com/shop', 
                         url_suffix = c('/sale/ski/mens/shipsto_us/online/'), 
                         url_pages = paste0('p_', seq(1, 10)))

paste0(crawl_urls$url_base[1], crawl_urls$url_suffix[1], 
       crawl_urls$url_pages[1])

# run initial crawl so that we can calculate how many pages
#   we need to ultimately read, also can serve as a test
init_crawl <- read_html(paste0(crawl_urls$url_base[1], 
                               crawl_urls$url_suffix[1], 
                               crawl_urls$url_pages[1]))

results_total <- init_crawl %>% html_elements('.results-count') %>% 
  html_text2()
results_total <- as.double(results_total[1])

# now remake the urls dataframe with the correct number of pages to crawl
crawl_urls <- data.frame(url_base = 'https://www.evo.com/shop', 
                         url_suffix = c('/sale/ski/mens/shipsto_us/online/'), 
                         url_pages = paste0('p_', 
                                            seq(1, 
                                                ceiling(results_total / 40))))
# crawl_urls

# cleanup !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ls()
trash()

# ^ -----

# crawling tests -----------------------------------------

# asdf <- read_html(paste0(crawl_urls$url_base, 
#                          crawl_urls$url_suffix, 
#                          crawl_urls$url_pages[1]))
# asdf <- init_crawl

# 0000000000000000000000000000000000000000000000000000000000000000
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

# 0000000000000000000000000000000000000000000000000000000000000000
# raw_df <- data.frame(product = asdf %>% 
#                        html_elements('.product-thumb-title') %>% 
#                        html_text2())
# raw_df <- cbind(raw_df, data.frame(rid = c(seq(1, nrow(raw_df)))))
# raw_df
# 
# interim_product <- rbind(raw_df, raw_df) %>% arrange(rid) %>% 
#   select(-rid)
# 
# raw_prices <- data.frame(all_prices = asdf %>% 
#                            html_elements('.product-thumb-price') %>% 
#                            html_text2(), 
#                          product = interim_product) %>% 
#   mutate(interim = row_number(), 
#          interim = ifelse(interim %% 2 == 0, 
#                           'discount_price', 'regular_price')) %>% 
#   pivot_wider(names_from = interim, values_from = all_prices)
# raw_prices %>% View()

# cleanup !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# rm(asdf, raw_df, raw_prices)
# trash()

# ^ -----

# crawling function -----------------------------------------------

fun_crawler <- function(url1, url2, url3, sleepy = 10) {
  # sleep here to avoid high freq spamming, give the server some rest
  Sys.sleep(sleepy + round(runif(1), 2))
  asdf <- read_html(paste0(url1, url2, url3))
  # start by pulling product names
  raw_df <- data.frame(product = asdf %>% 
                         html_elements('.product-thumb-title') %>% 
                         html_text2())
  raw_df <- cbind(raw_df, data.frame(rid = c(seq(1, nrow(raw_df)))))
  # this is a data cleaning quirck because all prices come out as 
  #   a single vector and what we actually need is 2 separate vectors
  #   one with the regular price and one with sale price
  interim_product <- rbind(raw_df, raw_df) %>% arrange(rid) %>% 
    select(-rid)
  raw_prices <- data.frame(all_prices = asdf %>% 
                             html_elements('.product-thumb-price') %>% 
                             html_text2(), 
                           product = interim_product) %>% 
    mutate(interim = row_number(), 
           interim = ifelse(interim %% 2 == 0, 
                            'discount_price', 'regular_price')) %>% 
    pivot_wider(names_from = interim, values_from = all_prices) %>% 
    mutate(url_page = url3)
  return(raw_prices)}

# expected runtime due to sleeping for full run of all pages
results_total %ps% ' total items'
(results_total / 40) %ps% ' pages of items'
(ceiling(results_total / 40) * 10 / 60) %ps% ' minutes of sleep'

# function tests ++++++++++++++++++++++++++++++++++++++++++
# test_crawl <- crawl_urls[13, ]
# 
# clockin()
# test_crawl_result <- pmap(.l = list(test_crawl$url_base,
#                                     test_crawl$url_suffix,
#                                     test_crawl$url_pages),
#                           .f = fun_crawler)
# clockout()
# test_crawl_result
# reduce(test_crawl_result, rbind)
# 
# rm(test_crawl, test_crawl_result)

# ^ -----

# run the crawler -------------------------------------------------

detach('package:tidylog', unload = TRUE)
clockin()
crawler_result <- pmap(.l = list(crawl_urls$url_base, 
                                 crawl_urls$url_suffix, 
                                 crawl_urls$url_pages), 
                       .f = fun_crawler)
clockout()
library(tidylog)
head(crawler_result)

# ^ -----

# reduce and clean up the crawler result -------------------------

# when crawling, some of the prices come out as list columns 
#   for a reason i cannot explain a hidden price is created 2 length 
#   list entires for certain records, and I visually verified that it 
#   is really just the first price we need, so pluck that from 
#   the list columns and make them char cols
fun_plucker <- function(df) {
  return_me <- df %>% mutate(regular_price = map_chr(regular_price, 
                                                     pluck, 1), 
                             discount_price = map_chr(discount_price, 
                                                      pluck, 1))
  return(return_me)}

# fix the list columns problem that comes up, then reduce list to 1 df
detach('package:tidylog', unload = TRUE)
clockin()
crawler_result <- map(crawler_result, fun_plucker) %>% 
  reduce(.f = rbind)
clockout()
library(tidylog)
crawler_result

# cleanup !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
rm(fun_crawler, fun_plucker)
ls()
trash()
dim(crawler_result)
sizer(crawler_result)
mem_used()

# additional metadata appending :::::::::::::::::::
output_df <- crawler_result %>% 
  mutate(crawled_runtime = metadatar$script_starttime, 
         crawled_url = paste0(crawl_urls$url_base[1], 
                              crawl_urls$url_suffix[1]))
output_df

# write to csv
filename <- paste0(getwd(), "/etl/ore/crawled",  
                   # paste(year(metadatar$script_starttime), 
                   #       month(metadatar$script_starttime),  
                   #       day(metadatar$script_starttime), 
                   #       hour(metadatar$script_starttime), 
                   #       minute(metadatar$script_starttime), sep = "-"), 
                   ".csv")
clockin()
write.csv(output_df, file = filename, row.names = FALSE)
clockout()

# ^ -----

