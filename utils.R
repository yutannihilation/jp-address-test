swap_comma <- function(x) stringr::str_replace_all(x, fixed("、"), "%%%comma%%%")
restore_comma <- function(x) stringr::str_replace_all(x, fixed("%%%comma%%%"), "、")

extract_variants <- function(x) {
  # str_subset(df$street, "\\(.*・.*\\)") %>% str_subset("地階・階層不明", negate = TRUE)
  # #>  [1] "野牛(稲崎平302番地・315番地、トクサ沢)"    "三里塚(御料牧場・成田国際空港内)"         
  # #>  [3] "戸山(3丁目18・21番)"                       "西瑞江(4丁目1~2番・10~27番、5丁目)"       
  # #>  [5] "釜ケ島(土手畑・藤場)"                      "大豆(1の2、3の2~6、4の2・4・6、11の1番地)"
  # #>  [7] "結東(逆巻・前倉・結東)"                    "北山(渋御殿湯・渋の湯)"                   
  # #>  [9] "中山町出渕(豊岡・東町)"                    "浦ノ内東分(鳴無・坂内)"                   
  # #> [11] "土居(甲・乙)"                              "牧(1~3丁目、白滝B・C、高見)"              
  # #> [13] "岡之原町(832の2・4、852の3)"
  ignore_idx <- x %in% c("(地階・階層不明)")
  
  # if x is in ignore_idx, return the original input as is
  x_orig <- x
  
  x <- x_orig[!ignore_idx]
  
  # remove head and tail parenthesis
  x <- str_remove_all(x, "^\\(|\\)$")
  
  # replace 、 inside 「」 to prevent them from being split
  x <- str_replace_all(x, "「.*?」", swap_comma)
  # split by 、
  x <- str_split(x, fixed("、"))
  
  x <- ifelse(ignore_idx, as.list(x_orig), x)
  
  x <- purrr::flatten_chr(x)
  # restore 、 inside 「」
  x <- restore_comma(x)
  x
}

extract_variants_one <- function(x) {
  if (x %in% ignore_list) {
    return(x)
  }
  
  x
}

# simple tests
local({
  testthat::expect_equal(extract_variants("(5614、5619、5647、5653番地)"),
                         c("5614番地", "5619番地", "5647番地", "5653番地"))
  testthat::expect_equal(extract_variants("(1~3丁目、白滝B・C、高見)"),
                         c("1丁目", "2丁目", "3丁目", "白滝B", "白滝C", "高見"))
})
