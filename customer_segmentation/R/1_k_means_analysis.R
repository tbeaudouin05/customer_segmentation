library(RODBC)
library(class)
library(XLConnect)

setwd('C:/Users/user/Google Drive/Bamilo Project Packs/Bamilo Open Project Packs/1_cross_department/2_customer_cube/data')

# run sql query ---------------------------------------------------------------------------------

conn <- odbcConnect("PM", uid = "Nasser", pwd = "naser123")

# fetch the text of the query 
unformatted_query <- readLines('sql_query.txt')

# format the text of the query so that it becomes readable by R 
#NB: THERE SHOULD BE NO COMMENT IN THE SQL or this will not work
#NBB: there should be NO UTF-8 characters! always use IDs for filtering
formatted_query <- paste(unformatted_query, collapse=" ")
formatted_query <- gsub("\t","", formatted_query)

# run the query on SQL Server database
query_output <- sqlQuery(conn, formatted_query)

# add columns to data ------------------------------------------------------------

data <- query_output

data$day_since_last_order <- as.numeric(difftime(data$last_refresh_date, data$last_order_date, units = 'days'))
data$month_since_first_order <- as.numeric(difftime(data$last_refresh_date, data$first_order_date, units = 'days')/30)
data$discount_ratio <- data$discount / data$nmv

data$nmv_eur <- data$nmv / 47000
data$avg_item_value_eur <- data$nmv_eur / data$item_count
data$avg_order_value_eur <- data$nmv_eur / data$order_count
data$order_discount_ratio <- data$discount_order_count / data$order_count
data$return_ratio <- data$return_item_count / data$item_count
data$cancel_ratio <- data$cancel_item_count / data$item_count
data$order_per_month <- ifelse(data$month_since_first_order < 1, data$order_count, data$order_count/data$month_since_first_order)
data$bad_experience_ratio <- (data$refund_reject_count + data$bad_cancel_reason_count + data$bad_return_reason_count) / data$item_count

# Exclude outliers ------------------------------------------------------------------

data <- data[data$avg_item_value_eur < quantile(data$avg_item_value_eur, 0.999), ]
data <- data[data$order_per_month < quantile(data$order_per_month, 0.9999), ]
data <- data[data$avg_order_value_eur < quantile(data$avg_order_value_eur, 0.9999), ]
data <- data[data$item_count < quantile(data$item_count, 0.9999), ]
data <- data[data$day_since_last_order < 180, ]

#normalize data -----------------------------------------------------------------------

data$avg_order_value_eur_zlog <- scale(log(data$avg_order_value_eur +0.1) +1)*0.1
data$avg_item_value_eur_zlog <- scale(log(data$avg_item_value_eur +0.1) +1)*0.1

data$return_ratio_zlog <- scale(log(data$return_ratio +0.1) +1)*1.5
data$cancel_ratio_zlog <- scale(log(data$cancel_ratio +0.1) +1)*1.7

data$item_count_female_zlog <- scale(log(data$item_count_female +0.1) +1)*1.5
data$item_count_male_zlog <- scale(log(data$item_count_male +0.1) +1)*1.5

data$item_count_20_zlog <- scale(log(data$item_count_20 +0.1) +1)*1.7
data$item_count_20_35_zlog <- scale(log(data$item_count_20_35 +0.1) +1)*1.5
data$item_count_35_50_zlog <- scale(log(data$item_count_35_50 +0.1) +1)*1.5
data$item_count_50_zlog <- scale(log(data$item_count_50 +0.1) +1)*1.7

data$item_count_tehran_zlog <- scale(log(data$item_count_tehran +0.1) +1)*2
data$item_count_shahrestan_zlog <- scale(log(data$item_count_shahrestan +0.1) +1)*2

data$order_per_month_zlog <- scale(log(data$order_per_month +0.1) +1)*2

data$item_count_electronic_accessories_zlog <- scale(log(data$item_count_electronic_accessories +0.1) +1)*4
data$item_count_mobile_tablet_zlog <- scale(log(data$item_count_mobile_tablet +0.1) +1)*4
data$item_count_home_living_zlog <- scale(log(data$item_count_home_living +0.1) +1)*4
data$item_count_fashion_zlog <- scale(log(data$item_count_fashion +0.1) +1)*4
data$item_count_health_beauty_zlog <- scale(log(data$item_count_health_beauty +0.1) +1)*4
data$item_count_other_gm_zlog <- scale(log(data$item_count_other_gm +0.1) +1)*4
data$item_count_fmcg_zlog <- scale(log(data$item_count_fmcg +0.1) +1)*4

data$bad_experience_ratio_zlog <- scale(log(data$bad_experience_ratio +0.1) +1)*2.8
data$order_discount_ratio_zlog <- scale(log(data$order_discount_ratio +0.1) +1)*2.8
data$order_count_zlog <- scale(log(data$order_count +0.1) +1)*3

#Null to 0 ---------------------------------------------------------------------

data[is.na(data)] <- 0

# kmeans algorithm --------------------------------------------------------------------

output <- kmeans(data[42:64], 80, iter.max = 400, nstart = 80, trace = TRUE)

# export data & centers  ------------------------------------------

data <-data[1:41]

data$cluster_id <- output$cluster

center <- output$centers

write.csv(data, 'data_with_cluster.csv')

write.csv(center, 'center.csv')

