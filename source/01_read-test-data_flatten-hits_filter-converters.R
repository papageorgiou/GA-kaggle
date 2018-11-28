library(tidyverse)
library(jsonlite)
library(stringr)

options(scipen=999)
options(digits=3)

col_types <- cols(
    channelGrouping = col_character(),
    customDimensions = col_character(),
    date = col_datetime(), # Parses YYYYMMDD
    device = col_character(),
    fullVisitorId = col_character(),
    geoNetwork = col_character(),
    hits = col_character(), # 
    socialEngagementType = col_skip(), #
    totals = col_character(),
    trafficSource = col_character(),
    visitId = col_integer(), # 
    visitNumber = col_integer(),
    visitStartTime = col_integer() # 
)

# Convert Python array/dictionary string to JSON format


unsnake <- . %>%
    str_replace_all(c("\\[\\]" = "[{}]", # empty element must contain dictionary
                      "^\\[|\\]$" = "", # remove initial and final brackets
                      "(\\[|\\{|, |: |', )'" = "\\1\"", # open single- to double-quote (on key or value)
                      "'(\\]|\\}|: |, )" = '\"\\1')) # close quote

separate_json <- . %>%
    str_replace_all(c("\"[A-Za-z]+\": \"not available in demo dataset\"(, )?" = "",
                      ", \\}" = "}")) %>% # if last property in list was removed
    paste(collapse = ",") %>% paste("[", ., "]") %>% # As fromJSON() isn't vectorised
    fromJSON(., flatten = TRUE)


df <- read_csv('D:/LargeData/kaggleupdated/test_v2.csv', col_types = col_types) %>% 
    bind_cols(separate_json(.$device))        %>% select(-device) %>%
    bind_cols(separate_json(.$geoNetwork))    %>% select(-geoNetwork) %>%
    bind_cols(separate_json(.$totals))        %>% select(-totals) %>%
    bind_cols(separate_json(.$trafficSource)) %>% select(-trafficSource) %>%
    bind_cols(separate_json(unsnake(.$customDimensions))) %>% select(-customDimensions) 

df_rev <- df %>% 
    mutate(transactionRevenue=as.numeric(transactionRevenue)) %>%  
    filter(transactionRevenue > 0) 

df_rev_users <- df %>% filter(fullVisitorId %in% df_rev$fullVisitorId)



#æŒ‰å“ ç‰Œè´\xadä¹  be | æŒ‰å“ ç‰Œè´\xadä¹°| Google Merchandise 

fix_json_hits <- function(jsonstring) {
    jsonstring %>%
        str_replace_all(c(
            '"' = "'", # there are few double quotes in the original, 
            #converted to single as a first step, for uniformity
            "'(?![a-z])|(?<=\\{|\\s)'" = '"', # converts single quotes to double, except apostrophes
            "True" = '"True"', # add double quotes to logical values
            '" : "' = '":"', # helper
            '(?<!: )"(?= |é)' = ""# remove last remaining double quotes
            
        ))
}




hit_type_df_users  <- df_rev_users %>% #filter(!grepl("xadä|xadç|\\+canva", hits)) %>% 
    rename(csv_hits = hits) %>%
    mutate(fixed_json_hits = fix_json_hits(csv_hits)) %>%
    mutate(r_object_hits = map(fixed_json_hits, possibly(fromJSON, NA_real_))) %>% 
    mutate(referer=map(r_object_hits, "referer")) %>% 
    mutate(action_type=map(r_object_hits, c("eCommerceAction", "action_type"))) %>% 
    select(-csv_hits, -fixed_json_hits, -r_object_hits) %>% 
    mutate(referer_1=map(referer, 1)) %>% 
    mutate(action_type_nu=map(action_type, as.numeric)) 

# possiby <- read_rds("hit_type_df_users_possibly.rds")


# prodsku_df <- hit_type_df %>%
#     mutate(prod=map(r_object_hits, "product"), 
#            prod_unlisted=map(prod, unlist), 
#            prodsku=map(prod_unlisted, "productSKU1")) 


