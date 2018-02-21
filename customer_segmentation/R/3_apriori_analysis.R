library(RODBC)
library(class)
library(XLConnect)
library(arules)
library(data.table)


setwd('D:/Gdrive/Bamilo Project Packs/Bamilo Open Project Packs/1_cross_department/2_customer_cube/data')
query_save <- list()

# REQUIRED: CLUSTER_CENTER FROM K-MEANS ANALYSIS

# main loop: fetch data from database, run knn analysis, join successive cluster_id to each customer and run successive apriori analyses ------------------------------------

i <- 1

for (year_month in c(20176,20179,201712)) {

# run sql query ---------------------------------------------------------------------------------
  
conn <- odbcConnect("PM", uid = "Nasser", pwd = "naser123")

# fetch the text of the query 
unformatted_query <- readLines('sql_query_loop.txt')

year <- substring(year_month,1,4)
month <- substring(year_month,5,6)

time_filter <- paste('WHERE YEAR(si.created_at)<=',year,' AND MONTH(si.created_at)<=',month)

# format the text of the query so that it becomes readable by R 
# and append the "where" clause with time_filter to the query
#NB: THERE SHOULD BE NO COMMENT IN THE SQL or this will not work
formatted_query <- paste(unformatted_query, collapse=" ")
formatted_query <- gsub("\t","", formatted_query)
formatted_query <- paste(formatted_query, 
time_filter,
'AND si.finance_verified_at IS NOT NULL ',
'AND so.customer_id not IN ',
'(\'246907\',\'133\',\'4986\',\'65236\',\'430\',\'98736\',\'11317\',\'142\',\'39760\',\'151383\',\'152188\',\'152190\',\'152192\',\'152193\',\'152194\',\'152196\',\'152208\',\'212430\',\'90213\',\'230403\',\'187008\',\'32783\',\'233882\',\'10945\',\'8725\',\'120481\',\'100689\',\'93107\',\'22100\',\'236087\',\'267516\',\'65928\',\'283470\',\'279310\',\'91255\',\'87762\',\'177852\',\'139068\',\'291658\',\'293187\',\'113785\',\'38053\',\'12795\',\'351241\',\'360043\',\'364758\',\'30131\',\'291748\',\'396300\',\'285739\',\'54884\',\'502676\',\'508462\',\'695\',\'312659\',\'527671\',\'178085\',\'551052\',\'245597\',\'305997\',\'563602\',\'572411\',\'581616\',\'391038\',\'3145\',\'608488\',\'418\',\'643\',\'112048\' )',
'AND CAST(so.customer_id as VARCHAR(MAX))!=\'98736 /*Test User*/\'',
'AND si.unit_price<500000000 ) il',
'GROUP BY il.customer_id')
formatted_query <- paste(formatted_query, collapse=" ")
formatted_query <- gsub("\t","", formatted_query)

# run the query on SQL Server database
query_output <- sqlQuery(conn, formatted_query)

query_save[[i]] <- query_output

# add columns to data ------------------------------------------------------------

data <- query_output

data$month_since_first_order <- as.numeric(difftime(data$last_refresh_date, data$first_order_date, units = 'days')/30)

data$nmv_eur <- data$nmv / 47000
data$avg_item_value_eur <- data$nmv_eur / data$item_count
data$avg_order_value_eur <- data$nmv_eur / data$order_count
data$order_discount_ratio <- data$discount_order_count / data$order_count
data$return_ratio <- data$return_item_count / data$item_count
data$cancel_ratio <- data$cancel_item_count / data$item_count
data$order_per_month <- ifelse(data$month_since_first_order < 1, data$order_count, data$order_count/data$month_since_first_order)
data$bad_experience_ratio <- (data$refund_reject_count + data$bad_cancel_reason_count + data$bad_return_reason_count) / data$item_count

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

data$order_per_month_zlog <- scale(log(data$order_per_month +0.1) +1)*5

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

# knn algorithm --------------------------------------------------------------------

train_set <- sqlQuery(conn, 'SELECT * FROM dbo.Cluster_Centers')

knn_pred <- knn(train = train_set[2:24] , test = data[38:60], cl = train_set[,1], k=1)

data$cluster_id_pred <- knn_pred


# fetch customer_id and cluster_id as numbers
customer_id <- as.double(data$customer_id)
cluster_id <- as.double(data$cluster_id_pred)

# prepare data to be merge
# adapt column name "cluster_id" to time period ex: cluster_id_2016_3

to_merge <- data.frame(customer_id, cluster_id)

names(to_merge)[2]<- paste('cluster_id', year_month , sep='_')
       

# merge new cluster ID with previous period cluster ID on customer ID
# unless it is first loop - then just return first cluster ID
if (i==1) {merged <- to_merge} 
else {merged <- merge(merged,to_merge, by = 'customer_id', all.y = TRUE)}

i <- i+1}

# run apriori analysis

i <- 2

for (year_month in c(20179,201712)) {
  
  
  max_corr_each_cluster <- list()
  j <- 1
  
  for (j_var in c(1:80)) {
    
    apriori_data <- merged
    apriori_data[is.na(apriori_data)] <- 0
    
    
    apriori_data <- apriori_data[apriori_data[,i] != 0,]
    apriori_data <- apriori_data[apriori_data[,i+1] != 0,]
    
    apriori_data <- apriori_data[apriori_data[,i] == j,]
    
    apriori_data <- data.frame(apriori_data[i], apriori_data[i+1])
    
    apriori_data <-  as.list(data.frame(t(apriori_data)))
    apriori_data <- as(apriori_data, 'transactions')
    result <- apriori(apriori_data, parameter = list(minlen=2, sup = 0.000000001, conf = 0.1, target="rules"))
    df_result <- as(result,"data.frame")
    df_result$dest_proba <- df_result$support * 100
    
    
    df_result$rules <- gsub("\\{","", df_result$rules)
    df_result$rules <- gsub("\\}","", df_result$rules)
    df_result$rules <- gsub("=>","", df_result$rules)
    df_result$rules <- gsub(j,"", df_result$rules)
    df_result$rules <- gsub(" ","", df_result$rules)
    
    names(df_result)[1]<- 'destination_cluster'
    
    df_result$origin_cluster <- j
    
    time_frame <- as.double(year_month)
    origin_cluster <- as.double(df_result$origin_cluster)
    destination_cluster <- as.double(df_result$destination_cluster)
    dest_proba <- as.double(df_result$support)
    dest_cust_count <- as.double(df_result$count)
    
    to_append <- data.frame(time_frame, origin_cluster, destination_cluster, dest_proba, dest_cust_count)
    
    if (j==1) {appended <- to_append
    appended <- data.table(appended) 
    appended <- appended[, .(mean(dest_proba), mean(dest_cust_count)), by=list(time_frame, origin_cluster,destination_cluster)]
    names(appended)[4]<- 'dest_proba'
    names(appended)[5]<- 'dest_cust_count'
    } else {appended <- rbind(appended,to_append)
    appended <- data.table(appended) 
    appended <- appended[, .(mean(dest_proba), mean(dest_cust_count)), by=list(time_frame, origin_cluster,destination_cluster)]
    names(appended)[4]<- 'dest_proba'
    names(appended)[5]<- 'dest_cust_count'}
    
    
    j <- j+1
    
  } 
  if (i==2) {output  <- appended} else {output <- rbind(output,appended)}
  
  
  
  i <- i+1}

# outside of main loop ----------------------------------------------------------------------------------

output_2 <- data.frame(output)
output_2 <- output_2[2:5]
output_2 <- data.table(output_2) 
output_2 <- output_2[, .(mean(dest_proba), mean(dest_cust_count)), by=list(origin_cluster,destination_cluster)]
names(output_2)[3]<- 'avg_dest_proba'
names(output_2)[4]<- 'avg_dest_cust_count'
output_2 <- data.frame(output_2)

for (k in c(1:80)){
  
  f_output_2 <- output_2[output_2[,1] == k,]
  f_output_2 <- f_output_2[order(-f_output_2$avg_dest_proba),]
  
  output_3 <- f_output_2[1:3,, drop=FALSE]
  
  
  if (k==1){final_output <- output_3} else {final_output <- rbind(final_output,output_3)}
  
}