swap_comma <- function(x) stringr::str_replace_all(x, fixed("、"), "%%%comma%%%")
restore_comma <- function(x) stringr::str_replace_all(x, fixed("%%%comma%%%"), "、")

split_pattern <- "、|・"

extract_variants <- function(x) {
  # str_subset(df$street, "\\(.*・.*\\)") %>% str_subset("地階・階層不明", negate = TRUE)
  # #>  [1] "野牛(稲崎平302番地・315番地、トクサ沢)"    "三里塚(御料牧場・成田国際空港内)"         
  # #>  [3] "戸山(3丁目18・21番)"                       "西瑞江(4丁目1~2番・10~27番、5丁目)"       
  # #>  [5] "釜ケ島(土手畑・藤場)"                      "大豆(1の2、3の2~6、4の2・4・6、11の1番地)"
  # #>  [7] "結東(逆巻・前倉・結東)"                    "北山(渋御殿湯・渋の湯)"                   
  # #>  [9] "中山町出渕(豊岡・東町)"                    "浦ノ内東分(鳴無・坂内)"                   
  # #> [11] "土居(甲・乙)"                              "牧(1~3丁目、白滝B・C、高見)"              
  # #> [13] "岡之原町(832の2・4、852の3)"
  ignore_idx <- x %in% c("(地階・階層不明)", "(110-2、110-7、110-10番地を除く)")
  
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

pattern_comma <- fixed("、")
pattern_cdot  <- fixed("・")
pattern_tilda <- fixed("~")
patterns <- fixed(c("、", "・", "~"))

extract_variants_one <- function(x) {
  i <- str_detect(x, patterns)

  # if x has no chance to split, return it as is
  if (!any(i)) {
    return(x)
  }
  
  # if x has 、, split by it first
  if (i[1]) {
    x <- str_split(x, pattern_comma)
    # if it should be split further, apply extract_variants_one() recursively
    if (i[2] || i[3]) {
      x <- purrr::map(x, extract_variants_one)
    }
    
    x <- purrr::flatten_chr(x)
    loc <- str_locate(x, )
    
    return(x)
  }

  # if x has ・ , split by it
  if (i[2]) {
    x <- str_split(x, pattern_cdot)
    # if it should be split further, apply extract_variants_one() recursively
    if (i[3]) {
      x <- purrr::map(x, extract_variants_one)
    }
    # flatten and return it
    return(purrr::flatten_chr(x))
  }
  
  # if x has ~, enumerate it
  if (i[3]) {
    enumerate_tilda(x)
  }
  
  # should not come here...
  stop("something is wrong!", call. = FALSE)
}

enumerate_tilda <- function(x) {
  x
}

# simple tests
local({
  testthat::expect_equal(extract_variants_one("1、2"), c("1", "2"))
  testthat::expect_equal(extract_variants_one("1、2丁目"), c("1丁目", "2丁目"))
  testthat::expect_equal(extract_variants_one("第3、4"), c("第3", "第4"))
  testthat::expect_equal(extract_variants_one("青井谷1、2丁目"), c("青井谷1丁目", "青井谷2丁目"))
  testthat::expect_equal(extract_variants_one("3丁目18・21番"), c("3丁目18番", "3丁目21番"))
  testthat::expect_equal(extract_variants_one("2丁目1番1~3号、2番"),
                         c("2丁目1番1号", "2丁目1番2号", "2丁目1番3号", "2丁目2番"))
  testthat::expect_equal(extract_variants_one("2丁目1、2、3番地、3丁目1、2番地"),
                         c("2丁目1番地", "2丁目2番地", "2丁目3番地", "3丁目1番地", "3丁目2番地"))
  testthat::expect_equal(extract_variants_one("1~3、5~6番地"),
                         c("1番地", "2番地", "3番地", "5番地", "6番地"))
  testthat::expect_equal(extract_variants_one("1-3、4、5-6番地"),
                         c("1-3番地", "4番地", "5-6番地"))
  testthat::expect_equal(extract_variants_one("4丁目1~2番・10~11番、5丁目"),
                         c("4丁目1番", "4丁目2番", "4丁目10番", "4丁目11番", "5丁目"))
  testthat::expect_equal(extract_variants_one("9820、9821、9823~9825、9864番地以上"),
                         c("9820番地", "9821番地", "9823番地", "9824番地", "9825番地", "9864番地以上"))
  testthat::expect_equal(extract_variants_one("1の2、3の2~4、4の2・4・6、11の1番地"),
                         c("1の2番地", "3の2番地", "3の3番地", "3の4番地", "4の2番地", "4の4番地", "4の6番地", "11の1番地"))
  testthat::expect_equal(extract_variants_one("1~3、5~6番地〔発電所構内〕"),
                         c("1番地〔発電所構内〕", "2番地〔発電所構内〕", "3番地〔発電所構内〕", "5番地〔発電所構内〕", "6番地〔発電所構内〕"))
  testthat::expect_equal(extract_variants_one("1番地~3番地、2757番地~2759番地"),
                         c("1番地", "2番地", "3番地", "2757番地", "2758番地", "2759番地"))
  testthat::expect_equal(extract_variants_one("13、14番地、10番地の2、5"),
                         c("13番地", "14番地", "10番地の2", "10番地の5"))
  testthat::expect_equal(extract_variants_one("1、2、3番地"), c("1番地", "2番地", "3番地"))

  testthat::expect_equal(extract_variants("(5614、5619、5647、5653番地)"),
                         c("5614番地", "5619番地", "5647番地", "5653番地"))
  testthat::expect_equal(extract_variants("(1~3丁目、白滝B・C、高見)"),
                         c("1丁目", "2丁目", "3丁目", "白滝B", "白滝C", "高見"))
  
  # exception  
  testthat::expect_equal(extract_variants("(地階・階層不明)"), "(地階・階層不明)")
  testthat::expect_equal(extract_variants("(110-2、110-7、110-10番地を除く)"), "(110-2、110-7、110-10番地を除く)")
})
