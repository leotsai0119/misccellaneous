
# author: CAI YUN-TING ----------------------------------------------------
# The Survey of Family Income and Expenditure, 2018 -----------------------

# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
# set working directory
setwd("i:/R_wd/tw_inc/")
# ins.pak
devtools::source_url("https://raw.githubusercontent.com/leotsai0119/misc/master/func_ins.pack.R")
# ins.pack
ins.pack("tidyverse", "docxtractr", "readtext", 
         "haven", "hablar"); rm(ins.pack)
# options
options(readr.show_progress = TRUE)
# do not show scientific notation
options(scipen = 999)
# timestamp
timestamp <- format(Sys.time(), "%m%d-%H%M")
# processing time
ptm <- proc.time()
# data source
path_code <- "AA170043/code107.docx"
path_dat <- "AA170043/inc107.dat"
year <- 107

# create the codebook -----------------------------------------------------

# codebook
# file is in the default working dirctory
code_tbl <- read_docx(path_code) %>% 
        docx_extract_tbl() %>% 
        .[complete.cases(.), ]
# add row: card_num
code_tbl <- rbind(code_tbl, c(NA, "card_num", "#/79-80", NA, NA, NA))
# colnames
colnames(code_tbl) <- c("q_num", "variable", "card_pos", 
                           "label", "level", "note")
# variable b1_# - b25_#
l <- grep("^b", code_tbl$variable)
code_tbl$variable[l] %<>% 
        str_split("#", simplify = TRUE) %>%
        .[ , 1] %>% as.character()
rm(l)

# start
code_tbl$start <- code_tbl$card_pos %>% 
        str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% 
        .[ ,1] %>% as.integer()
# end
code_tbl$end <- code_tbl$card_pos %>% 
        str_split("/", simplify = TRUE) %>% 
        .[ , 2] %>% str_split("-", simplify = TRUE) %>% 
        .[ ,2] %>% as.integer()
# replace NA in end
code_tbl$end <- with(code_tbl, if_else(is.na(end), start, end))

# list for grep (grepbook) ------------------------------------------------

sym <- c("{", grep("[A-R]", LETTERS, value = TRUE), "}")
digi <- c(0:9, 1:9, 0)
positive <- c(rep("+", 10), rep("-", 10))
grepbook <- tibble(sym, digi, positive)
# pattern for grep and gsub
pattern <- grepbook$sym %>% .[2:19]
grepbook$pattern <- c("\\{$", sapply(pattern, paste, "$", sep = ""), "\\}$")

# names of item_xxx -------------------------------------------------------

doc.text.parts <- readtext(path_code)$text %>% 
        strsplit("\n") %>% .[[1]]
# items
doc.items <- doc.text.parts %>% 
        # begin with at least two digits
        grep("^[1-9][0-9]{1,3}:", ., value = TRUE) %>% 
        strsplit(., ":") %>% 
        unlist() 
        
# item numbers
doc.items.digits <- doc.items %>% 
        .[2 * (1:length(.)) - 1] %>% 
        .[!is.na(.)]
# item contents
doc.items.contents <- doc.items %>% 
        .[2 * (1:length(doc.text.parts))] %>% 
        .[!is.na(.)]


# data processing and manipulation ----------------------------------------
# data raw and card_num
df.source <- read_fwf(path_dat, fwf_positions(start = c(1, 79), 
                                              end = c(80, 80), 
                                              col_names = c("raw", "card_num")
                                              ), 
                      # card_num as integer
                      col_types = cols(card_num = "i", .default = "c"))

# card 01 -----------------------------------------------------------------
# filter out card_num == 1
x <- filter(df.source, card_num == 1) %>% .[ ,1] %>% .$raw
# define tempfile name and format
y <- tempfile("tp", fileext = ".dat")
# write the tempfile
write(x, file = y)
# read card 1
# code_tbl[2:25]
l <- grep("x1|area|stage|id|^a", code_tbl$variable)
df1 <- read_fwf(y, fwf_positions(code_tbl$start[l], 
                                 code_tbl$end[l],
                                 col_names = code_tbl$variable[l]), 
                # define column types (variable classes) in df1
                col_types = cols(x1 = "c", 
                                 id = "c", a4 = "f", 
                                 a5 = "f", a6 = "n", a7 = "f", 
                                 a8 = "n", a9 = "n", a11 = "f", 
                                 a12 = "n", a13 = "n", a16 = "f", 
                                 a17 = "f", a18 = "f", a19 = "n", 
                                 a20 = "n")) %>% 
        # order data
        .[order(.$x1), ]
# free up ram
gc()


# card 02 -----------------------------------------------------------------
# card_num 02:20
# function f2
f2 <- function(c, d = c - 1) {
        # if input c (card_num) is not in the raw data, 
        # then create a temp file wiith "000000001" (for merging data), 
        # which will fill NA.
        # matrix with 29 columns c(2, 26:54)
        if(c %in% df.source$card_num) {
                # filter out card_num == 02:20 and create a temporary .dat file
                x <- filter(df.source, card_num == c) %>% .[ ,1] %>% .$raw
                y <- tempfile("tmp", fileext = ".dat")
                write(x, file = y)
                # read file [2, 26:54]
                l <- grep("^x1|^b", code_tbl$variable)
                tmp <- read_fwf(y, fwf_positions(code_tbl$start[l], 
                                                 code_tbl$end[l]))
                } else {l <- grep("^x1|^b", code_tbl$variable)
                        tmp <- matrix(ncol = length(l)) %>% 
                        as_tibble(.name_repair = NULL)
                        tmp[ , 1] <- "00000001"        
                        }
        # name the columns (b1_1, b1_2, b1_3 ......)
        # eg. b1_# , card_num == 2, then # == 1, get b1_1 (#: 1:19)
        l <- grep("^x1|^b", code_tbl$variable)
        colnames(tmp) <- c("x1", paste(code_tbl$variable[l[-1]], d, sep = ""))
        return(tmp)
        }
# for loop and left_join (dplyr) 
# card number = 2:20
df2 <- list() 
for(i in 2:20) {
        df2[[i - 1]] <- f2(i)
        }
# left_joing with reduce
df2 <- Reduce(function(...) left_join(..., by = "x1"), df2)
# column types
# b1_, b4_, b21_, b23_, b25_
df2 <- df2 %>% convert(chr(x1),
                       num(contains("b4_")))
# b2_, b3_ ... (factor)
variables <- colnames(df2)
l <- grep("x1|b4", variables)
# mutate_if
df2[ , variables[-l]] %<>% mutate_if(is.character, as.factor) %>% 
        mutate_if(is.numeric, as.factor)
# free up ram
gc()

# card 21 -----------------------------------------------------------------
# filter out card_num == 21
x <- filter(df.source, card_num == 21) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
l <- grep("x1|^f", code_tbl$variable)
df21 <- read_fwf(y, fwf_positions(code_tbl$start[l], 
                                  code_tbl$end[l],
                                  # variable names
                                  col_names = code_tbl$variable[l]), 
                 # define column types
                 cols(x1 = "c", f57 = "f", 
                      f61 = "f", .default = "n")
                 ) %>% 
        # order by x1
        .[order(.$x1), ]

# free up ram
gc()


# card 22 -----------------------------------------------------------------
# filter out card_num == 22
x <- filter(df.source, card_num == 22) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
# code_tbl[86:112]
l <- grep("x1|^c[0-9]|^d[0-9]", code_tbl$variable)
df22 <- read_fwf(y, fwf_positions(code_tbl$start[l], 
                                  code_tbl$end[l],
                                  # variable names
                                  col_names = code_tbl$variable[l]), 
                 # define column types
                 col_types = cols(x1 = "c", 
                                  c1 = "f", c2 = "f", c4 = "f", 
                                  d1 = "f", d5 = "f", d6 = "f", 
                                  .default = "n")) %>% 
        # order by x1
        .[order(.$x1), ]

# free up ram
gc()


# card 23-99 --------------------------------------------------------------
# filter out card_num %in% 23:99
x <- filter(df.source, card_num %in% 23:99) %>% .[ ,1] %>% .$raw
y <- tempfile("tmp", fileext = ".dat")
write(x, file = y)
x <- list()
# for loop (5 sections)
# item 101 = "1010" (9, 12) , take posistion (9, 11)
for(i in 0:4) {
        x[[i + 1]] <- read_fwf(y, fwf_positions(c(1, 9 + i * 14, 13 + i * 14),
                                            c(8, 12 + i * 14, 22 + i * 14),
                                            col_names = c("x1", "item", "exp")), 
                               col_types = cols(x1 = "c", item = "c", exp = "c")
                           )
        df23 <- do.call(bind_rows, x) %>% distinct()
        }
# free up ram
gc()

# replace symbols with digits ---------------------------------------------

p <- grepbook$pattern
r <- grepbook$digi
for(i in 1:10) {
        # postitive [1:10]
        a <- grep(p[i], df23$exp)
        df23$exp[a] <- gsub(pattern = p[i], 
                            replacement = r[i], 
                            x = grep(p[i], df23$exp, value = TRUE))
        # negative [11:20]
        b <- grep(p[i + 10], df23$exp)
        df23$exp[b] <- gsub(pattern = p[i + 10], 
                            replacement = r[i + 10], 
                            x = grep(p[i + 10], df23$exp, value = TRUE)) %>% 
                paste("-", ., sep = "")
        }

# spread (transpose)
df23 <- df23 %>% spread(key = "item", value = "exp")
# remove column `0000`
df23 <- df23 %>% select( - one_of("0000"))

# items without observations ----------------------------------------------
# names of all the items
colnames(df23)[-1] <- colnames(df23)[-1] %>% 
        as.integer() %>% 
        paste("itm", ., sep = "")
itms_all <- doc.items.digits %>% 
        as.integer() %>% 
        paste("itm" , ., sep = "")
# create a tibble for those who are not in df23
df.itm.all <- matrix("NA", nrow = nrow(df23), ncol = length(itms_all)) %>% 
        as_tibble(.name_repair = NULL)
# create x1 column for merging
df.itm.all$x1 <- df23$x1
# name the columns with all item names
colnames(df.itm.all) <- c(itms_all, "x1")
# merge
df23 <- df23 %>% left_join(df.itm.all) %>% 
        # column types (hablar::convert)
        convert(chr(x1), num(contains("itm"))) %>% 
        # order
        .[order(.$x1), ]
# free up ram
gc()

# merge -------------------------------------------------------------------
data.list <- list(df1, df2, df21, df22, df23)
df.inc <- Reduce(function(...) left_join(..., by = "x1"), data.list)
# add year column
df.inc$year <- year
#
df.inc107 <- df.inc
code_tbl_107 <- code_tbl
# remove
rm(df.source, x, df.itm.all, 
   df1, df2, df21, df22, df23, 
   data.list, df.inc, code_tbl)
# free up memory
gc()


# time --------------------------------------------------------------------

proc.time() - ptm

# save --------------------------------------------------------------------
# .RData
# save df.inc
save(df.inc107, file = "c:/Users/user/Desktop/df_inc107.RData")
save(df.inc107, file = "R data files/df_inc107.RData")
# save code_tbl
save(code_tbl_107, file = "AA170043/code_tbl_107.RData")
save(code_tbl_107, file = "R data files/code_tbl_107.RData")
# .csv format
# write_csv(df.inc107, "inc107.csv", col_names = TRUE, na = "")
# .sas7bdat format
# write_sas(df.inc107, "inc107.sas7bdat")
# .sav format
# write_sav(df.inc107, "inc107.sav", compress = TRUE)


# time --------------------------------------------------------------------

proc.time() - ptm


# remove all objects ------------------------------------------------------

l <- grep("^df.inc|^code_tbl", ls())
rm(list = ls()[-l])