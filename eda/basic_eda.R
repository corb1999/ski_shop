# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-11-26"), 
                                    author = "corb", 
                                    proj_name = "ski_shop", 
                                    script_type = "eda", 
                                    notepad = paste0("basic query explore")), 
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
library(gt)
library(DBI)
library(RSQLite)
library(patchwork)
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

# query the sqlite database --------------------------------------

connection_db <- dbConnect(SQLite(), 
                           dbname = paste0(getwd(), 
                                           '/etl/db_sqlite', 
                                           '/ski_shop_db.db'))
dbListTables(connection_db)

sql_to_run <- dbSendQuery(connection_db, 'SELECT * FROM view_ultra;')
df <- dbFetch(sql_to_run)
df <- df %>% as_tibble()
dim(df)
colnames(df)
df

# ^ -----

# plot setup -----------------------------------------------------

(pltname <- 'EVOgear Sales ' %ps% 
   'as of ' %ps% max(df$crawled_runtime) %ps% '; ' %ps% 
   'Current-Records; ' %ps% 
   # 'No Packages; ' %ps% 
   # 'Skis; ' %ps% 
   '')

dfplt <- df %>% 
  filter(latest_crawl_run_ind == TRUE) %>%
  # filter(products_in_package == 1) %>% 
  # filter(tag_skis == 1) %>% 
  filter(product != '')

# ^ -----

# vizualization functions -------------------------------------

# simple overlay histograms of regular and discounted prices
fun_plt1 <- function(dff = dfplt) {
  plt1 <- dff %>% ggplot() + 
    geom_histogram(aes(x = regular_price_val), 
                   bins = 30, fill = 'black', 
                   color = 'black', alpha = 0.2) + 
    geom_histogram(aes(x = discount_price_val), 
                   bins = 30, fill = 'blue', 
                   color = 'white', alpha = 0.5) + 
    geom_vline(aes(xintercept = mean(regular_price_val))) + 
    geom_vline(aes(xintercept = mean(discount_price_val)), 
               color = 'blue', size = 1.1) + 
    theme_minimal() + 
    scale_x_continuous(labels = dollar_format()) + 
    labs(x = 'Product Price', y = 'Item Count', 
         caption = 'Black = regular price; Blue = discount price')
  return_me <- plt1 + plot_annotation(title = pltname)
  return(return_me)}
# fun_plt1()

# scatterplot of regular prices and the discount amount
fun_plt2 <- function(dff = dfplt) {
  plt1 <- dff %>% 
    ggplot(aes(x = regular_price_val, 
               y = (1- delta_price_percent))) + 
    geom_point(alpha = 0.5) + 
    theme_minimal() + 
    scale_x_continuous(labels = dollar_format()) + 
    scale_y_continuous(labels = percent_format()) + 
    labs(x = 'Product Regular Price', y = 'Discount Percent')
  return_me <- plt1 + plot_annotation(title = pltname)
  return(return_me)}
# fun_plt2()


# ^ -----

# basic viz --------------------------------------------------

fun_plt1()
fun_plt2()



# ^ -----


