trainv2 <- read_csv("Data/trainv2_2_flat.csv")

trainv2$visitStartTime = lubridate::as_datetime(trainv2$visitStartTime)
trainv2$totals.transactionRevenue=as.numeric(trainv2$totals.transactionRevenue)

converters <- trainv2 %>% mutate(totals.transactionRevenue=as.numeric(totals.transactionRevenue)) %>% filter(totals.transactionRevenue > 0) %>% distinct(fullVisitorId) 



# trainv2_converters <- trainv2 %>% 
#     mutate(totals.transactionRevenue=ifelse(is.na(totals.transactionRevenue), 0, totals.transactionRevenue)) %>% 
# 
#     filter(fullVisitorId %in% converters$fullVisitorId) %>%  # %>% mutate(converted=totals.transactionRevenue>0) %>% 
#     group_by(fullVisitorId) %>% summarise(totalTrans_before=sum(totals.transactionRevenue >0),
#                                        sumTrans_before=sum(totals.transactionRevenue))



trainv2_converters_fromMay17 <- trainv2 %>% filter(visitStartTime >= as_datetime("2017-05-01")) %>%
    filter(fullVisitorId %in% converters$fullVisitorId) %>%
    mutate(totals.transactionRevenue=ifelse(is.na(totals.transactionRevenue), 0, totals.transactionRevenue)) %>% 
      # %>% mutate(converted=totals.transactionRevenue>0) %>% 
    group_by(fullVisitorId) %>% summarise(totalTrans_before=sum(totals.transactionRevenue >0),
                                          sumTrans_before=sum(totals.transactionRevenue))



trainv2_converters_fromJul17 <- trainv2 %>% filter(visitStartTime >= as_datetime("2017-07-01")) %>% 
    filter(fullVisitorId %in% converters$fullVisitorId) %>%
    mutate(totals.transactionRevenue=ifelse(is.na(totals.transactionRevenue), 0, totals.transactionRevenue)) %>% 
      # %>% mutate(converted=totals.transactionRevenue>0) %>% 
    group_by(fullVisitorId) %>% summarise(totalTrans_before=sum(totals.transactionRevenue >0),
                                          sumTrans_before=sum(totals.transactionRevenue))



# write_csv(trainv2_converters_fromJul17, "trainv2_converters_fromJul17.csv")
# write_csv(trainv2_converters_fromMay17, "trainv2_converters_fromMay17.csv")
