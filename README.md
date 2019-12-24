JP Addresses
================

Most of the code here is from
<https://uribo.hatenablog.com/entry/2019/12/22/102452>. Thanks
[@uribo](https://github.com/uribo)\!

### Get data

``` r
dir.create(data_dir, showWarnings = FALSE)
temp <- tempfile(fileext = ".zip")
download.file("https://www.post.japanpost.jp/zipcode/dl/oogaki/zip/ken_all.zip", temp)
unzip(temp, exdir = data_dir)
```

### Load data

``` r
library(dplyr, warn.conflicts = FALSE)
library(zipangu)
library(stringr)

df <- read_zipcode(path = ken_all, type = "kogaki") %>%
  select(jis_code, zip_code, prefecture, city, street, is_cyoumoku) %>% 
  tibble::rowid_to_column()

nrow(df)
```

    ## [1] 124351

``` r
head(df)
```

    ## # A tibble: 6 x 7
    ##   rowid jis_code zip_code prefecture city         street             is_cyoumoku
    ##   <int> <chr>    <chr>    <chr>      <chr>        <chr>                    <dbl>
    ## 1     1 01101    0640941  北海道     札幌市中央区 旭ケ丘                       1
    ## 2     2 01101    0600041  北海道     札幌市中央区 大通東                       1
    ## 3     3 01101    0600042  北海道     札幌市中央区 大通西(1~19丁目)             1
    ## 4     4 01101    0640820  北海道     札幌市中央区 大通西(20~28丁目)            1
    ## 5     5 01101    0600031  北海道     札幌市中央区 北一条東                     1
    ## 6     6 01101    0600001  北海道     札幌市中央区 北一条西(1~19丁目)           1

### Duplicated columns

``` r
df %>% 
  group_by(zip_code, city, street) %>% 
  filter(n() > 1)
```

    ## # A tibble: 4 x 7
    ## # Groups:   zip_code, city, street [2]
    ##   rowid jis_code zip_code prefecture city   street is_cyoumoku
    ##   <int> <chr>    <chr>    <chr>      <chr>  <chr>        <dbl>
    ## 1 87020 27212    5810027  大阪府     八尾市 八尾木           1
    ## 2 87034 27212    5810027  大阪府     八尾市 八尾木           0
    ## 3 89850 28203    6730012  兵庫県     明石市 和坂             0
    ## 4 89923 28203    6730012  兵庫県     明石市 和坂             1

``` r
df <- df %>% 
  group_by(zip_code, city, street) %>% 
  filter(row_number() == 1) %>%
  ungroup()
```

### Multi-line columns

``` r
sum(str_count(df$street, fixed("(")))
```

    ## [1] 6329

``` r
sum(str_count(df$street, fixed(")")))
```

    ## [1] 6329

``` r
# 全角
sum(str_count(df$street, fixed("（")))
```

    ## [1] 0

``` r
sum(str_count(df$street, fixed("）")))
```

    ## [1] 0

``` r
df_marked <- df %>%
  mutate(
    opening_paren = cumsum(str_count(street, fixed("("))),
    closing_paren = cumsum(str_count(street, fixed(")"))),
    # increase id when
    # 1) the last line is a single-line row
    # 2) the last line is the end of multi-line rows
    row_group_id  = cumsum(lag(opening_paren == closing_paren, default = 0))
  )

# multi-line rows
df_marked %>%
  group_by(row_group_id) %>% 
  filter(n() > 1) %>% 
  head()
```

    ## # A tibble: 6 x 10
    ## # Groups:   row_group_id [3]
    ##   rowid jis_code zip_code prefecture city  street is_cyoumoku opening_paren
    ##   <int> <chr>    <chr>    <chr>      <chr> <chr>        <dbl>         <int>
    ## 1  3089 01224    0660005  北海道     千歳市… 協和(88…           0           167
    ## 2  3090 01224    0660005  北海道     千歳市… 3、431…           0           167
    ## 3  3091 01224    0660005  北海道     千歳市… 5、113…           0           167
    ## 4  3436 01230    0590005  北海道     登別市… 札内町(5…           0           188
    ## 5  3437 01230    0590005  北海道     登別市… 184、2…           0           188
    ## 6  3779 01303    0613774  北海道     石狩郡当… 川下(78…           0           202
    ## # … with 2 more variables: closing_paren <int>, row_group_id <dbl>

Confirm `jis_code`, `zip_code`, `prefecture`, `city` and `is_cyoumoku`
are the same value within the same `row_group_id`.

``` r
nrow(distinct(df_marked, row_group_id))
```

    ## [1] 124042

``` r
nrow(distinct(df_marked, jis_code, zip_code, prefecture, city, is_cyoumoku, row_group_id))
```

    ## [1] 124042

``` r
df <- df_marked %>%
  group_by(jis_code, zip_code, prefecture, city, is_cyoumoku, row_group_id) %>%
  # ensure the rows are in the original order
  arrange(rowid) %>%
  summarise(street = paste(street, collapse = ""), n = n()) %>% 
  ungroup()

# check if the streets are concatinated fine
df %>%
  filter(n > 1) %>%
  select(zip_code, street) %>% 
  head()
```

    ## # A tibble: 6 x 2
    ##   zip_code street                                                               
    ##   <chr>    <chr>                                                                
    ## 1 0660005  協和(88-2、271-10、343-2、404-1、427-3、431-12、443-6、608-2、641-8、814、842-5、…
    ## 2 0590005  札内町(5、9、11-12、36、42-2、62、80、95、184、231、389-2、499、500番地)…
    ## 3 0613774  川下(782-13、5363-7~8、5382-3、5405-4、5407-5、5445~5446-4番地)      
    ## 4 0482331  大江(2丁目651、662、668番地、3丁目103、118、210、254、267、372、444、469番地)…
    ## 5 0482402  大江(1丁目、2丁目「651、662、668番地」以外、3丁目5、13-4、20、678、687番地)…
    ## 6 0882686  俣落(1629-1、1653-3、1684-5、1886-2、1893、1896-2、1896-3、1906-8、1932-2、1936…

``` r
# remove unncessary columns
df <- df %>% 
  select(jis_code, zip_code, prefecture, city, is_cyoumoku, street)
```

### Extract streets

Which columns have multiple parenthesis?

``` r
str_subset(df$street, "\\(.*\\(")
```

    ##  [1] "名駅ミッドランドスクエア(高層棟)(1階)"           
    ##  [2] "名駅ミッドランドスクエア(高層棟)(2階)"           
    ##  [3] "名駅ミッドランドスクエア(高層棟)(3階)"           
    ##  [4] "名駅ミッドランドスクエア(高層棟)(4階)"           
    ##  [5] "名駅ミッドランドスクエア(高層棟)(5階)"           
    ##  [6] "名駅ミッドランドスクエア(高層棟)(6階)"           
    ##  [7] "名駅ミッドランドスクエア(高層棟)(7階)"           
    ##  [8] "名駅ミッドランドスクエア(高層棟)(8階)"           
    ##  [9] "名駅ミッドランドスクエア(高層棟)(9階)"           
    ## [10] "名駅ミッドランドスクエア(高層棟)(10階)"          
    ## [11] "名駅ミッドランドスクエア(高層棟)(11階)"          
    ## [12] "名駅ミッドランドスクエア(高層棟)(12階)"          
    ## [13] "名駅ミッドランドスクエア(高層棟)(13階)"          
    ## [14] "名駅ミッドランドスクエア(高層棟)(14階)"          
    ## [15] "名駅ミッドランドスクエア(高層棟)(15階)"          
    ## [16] "名駅ミッドランドスクエア(高層棟)(16階)"          
    ## [17] "名駅ミッドランドスクエア(高層棟)(17階)"          
    ## [18] "名駅ミッドランドスクエア(高層棟)(18階)"          
    ## [19] "名駅ミッドランドスクエア(高層棟)(19階)"          
    ## [20] "名駅ミッドランドスクエア(高層棟)(20階)"          
    ## [21] "名駅ミッドランドスクエア(高層棟)(21階)"          
    ## [22] "名駅ミッドランドスクエア(高層棟)(22階)"          
    ## [23] "名駅ミッドランドスクエア(高層棟)(23階)"          
    ## [24] "名駅ミッドランドスクエア(高層棟)(24階)"          
    ## [25] "名駅ミッドランドスクエア(高層棟)(25階)"          
    ## [26] "名駅ミッドランドスクエア(高層棟)(26階)"          
    ## [27] "名駅ミッドランドスクエア(高層棟)(27階)"          
    ## [28] "名駅ミッドランドスクエア(高層棟)(28階)"          
    ## [29] "名駅ミッドランドスクエア(高層棟)(29階)"          
    ## [30] "名駅ミッドランドスクエア(高層棟)(30階)"          
    ## [31] "名駅ミッドランドスクエア(高層棟)(31階)"          
    ## [32] "名駅ミッドランドスクエア(高層棟)(32階)"          
    ## [33] "名駅ミッドランドスクエア(高層棟)(33階)"          
    ## [34] "名駅ミッドランドスクエア(高層棟)(34階)"          
    ## [35] "名駅ミッドランドスクエア(高層棟)(35階)"          
    ## [36] "名駅ミッドランドスクエア(高層棟)(36階)"          
    ## [37] "名駅ミッドランドスクエア(高層棟)(37階)"          
    ## [38] "名駅ミッドランドスクエア(高層棟)(38階)"          
    ## [39] "名駅ミッドランドスクエア(高層棟)(39階)"          
    ## [40] "名駅ミッドランドスクエア(高層棟)(40階)"          
    ## [41] "名駅ミッドランドスクエア(高層棟)(41階)"          
    ## [42] "名駅ミッドランドスクエア(高層棟)(42階)"          
    ## [43] "名駅ミッドランドスクエア(高層棟)(43階)"          
    ## [44] "名駅ミッドランドスクエア(高層棟)(44階)"          
    ## [45] "名駅ミッドランドスクエア(高層棟)(45階)"          
    ## [46] "名駅ミッドランドスクエア(高層棟)(46階)"          
    ## [47] "名駅ミッドランドスクエア(高層棟)(47階)"          
    ## [48] "名駅ミッドランドスクエア(高層棟)(地階・階層不明)"

These rows don’t need to be extracted, so let’s ignore them.

``` r
library(tidyr)

df_extracted <- df %>%
  extract(street, c("base", "variants"), "^([^\\(]+)(\\([^\\(]*[、〜][^\\(]*\\))?$")

df_extracted %>% 
  filter(!is.na(variants)) %>% 
  head
```

    ## # A tibble: 6 x 7
    ##   jis_code zip_code prefecture city     is_cyoumoku base    variants            
    ##   <chr>    <chr>    <chr>      <chr>          <dbl> <chr>   <chr>               
    ## 1 01106    0050840  北海道     札幌市南区…           0 藤野    (400、400-2番地)    
    ## 2 01207    0800029  北海道     帯広市             1 西十九条南… (35~38、41、42丁目) 
    ## 3 01207    0800848  北海道     帯広市             1 自由が丘… (1、2丁目)          
    ## 4 01210    0683161  北海道     岩見沢市           0 栗沢町宮村… (248、339、726、780、80…
    ## 5 01213    0591366  北海道     苫小牧市           1 あけぼの町… (1、2丁目)          
    ## 6 01214    0984581  北海道     稚内市             0 抜海村  (上勇知、下勇知、夕来、オネトマナイ)…

``` r
df_extracted <- df_extracted %>% 
  filter(!is.na(variants)) %>% 
  # remove head and tail parenthesis
  mutate(variants = str_remove_all(variants, "^\\(|\\)$")) %>%
  # extract 丁目|番地
  extract(variants, c("variants", "unit"), "^(.*?)(丁目|番地)?$")
```

``` r
swap_comma <- function(x) stringr::str_replace_all(x, fixed("、"), "%%%comma%%%")
restore_comma <- function(x) stringr::str_replace_all(x, fixed("%%%comma%%%"), "、")

df_extracted <- df_extracted %>% 
  # replace 、 inside 「」 to prevent them from being split
  mutate(variants = str_replace_all(variants, "「.*?」", swap_comma)) %>% 
  # split by 、
  separate_rows(variants, sep = "、") %>% 
  # restore 、 inside 「」
  mutate(variants = restore_comma(variants))

# check the contents
str_subset(df_extracted$variants, "「|」")
```

    ##  [1] "2丁目「651、662、668番地」以外"                                                 
    ##  [2] "下久保「174を除く」"                                                            
    ##  [3] "坪毛沢「25、637、641、643、647を除く」"                                         
    ##  [4] "今熊「213~234、240、247、262、266、275、277、280、295、1199、1206、1504を除く」"
    ##  [5] "上折茂「1-13、71-192を除く」"                                                   
    ##  [6] "第40地割「57番地125、176を除く」~第45地割"                                      
    ##  [7] "430番地以上「1770-1~2、1862-42、1923-5を除く」"                                 
    ##  [8] "烏帽子「榛名湖畔」"                                                             
    ##  [9] "1~500「211番地を除く」「古町」"                                                 
    ## [10] "2527~2529「土遠」"                                                              
    ## [11] "250~343番地「255、256、258、259、262、276、294~300、302~304番地を除く」"        
    ## [12] "中一里山「9番地の4、12番地を除く」"                                             
    ## [13] "中一里山「9番地の4、12番地」"
