# Load required libraries
library(sqldf)
library(dplyr)
library(lubridate)

# ?????????????????????????????
library(scorecard)
library(gsubfn)
library(proto)
library(RSQLite)
library(partykit)
library(grid)
library(libcoin)
library(mvtnorm)
library(Formula)
library(smbinning)
library(zoo)
library(jsonlite)
library(tidyr)
library(purrr)
library(data.table)
library(lubridate)
library(tidyverse)
library(DataExplorer)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sqldf)

df_payment_rlvl <- read.csv("~/Desktop/ifungames/20230809/订单明细表.csv")


library(dplyr)
library(lubridate)


library(dplyr)

df_result <- df_payment_rlvl %>%
  mutate(register_time = as.POSIXct(register_time),
         first_pay_time = as.POSIXct(first_pay_time)) %>%
  filter(register_time >= '2023-07-01' & register_time < '2023-07-31' &
           as.numeric(difftime(first_pay_time, register_time, units = "days")) <= 7)  %>%
  group_by(account_id) %>%
  summarize(
    D7_avg_amount = mean(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 7, pay_amount, NA)),
    D3_avg_amount = mean(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 3, pay_amount, NA)),
    D3_sum_amount = sum(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 3, pay_amount, NA)),
    D7_sum_amount = sum(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 7, pay_amount, NA)),
    D7_max_amount = max(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 7, pay_amount, NA)),
    D7_pay_cnt = sum(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 7, 1, 0)),
    D3_pay_cnt = sum(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 3, 1, 0)),
    max_sub_day = max(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 7, as.numeric(difftime(ts, register_time, units = "days")), NA)),
    min_sub_day = min(ifelse(as.numeric(difftime(ts, register_time, units = "days")) <= 7, as.numeric(difftime(ts, register_time, units = "days")), NA)),
    min_pay_hour = min(as.numeric(difftime(first_pay_time, register_time, units = "hours"))),
    max_pay_hour = max(as.numeric(difftime(ts, register_time, units = "hours")))
  )





sqldf("select distinct b.account_id,b.register_time,b.ts,b.first_pay_time from df_result a join df_payment_rlvl b on(a.account_id = b.account_id) where a.min_pay_hour<0")

# Replace missing values with 0 in the result data frame
df_result[is.na(df_result)] <- 0



get_clust_tendency(df_result,40,graph = TRUE)

gap_stat <- clusGap(df_result,FUN = kmeans,nstart = 25,K.max = 10,B = 500)

fviz_gap_stat(gap_stat)
# Print the modified result
print(df_result)


# Select the columns you want to use for clustering
clustering_data <- df_result[, c("D7_sum_amount", 
                                 "D7_max_amount",
                                 "D7_pay_cnt", 
                                 "max_sub_day",
                                 "min_sub_day")]


fviz_gap_stat(gap_stat)
# Print the modified result
print(df_result)
# Perform k-means clustering
k <- 4  # Number of clusters
kmeans_result <- kmeans(clustering_data, centers = k,nstart = 75)

# Add cluster labels to the result
df_result$cluster <- kmeans_result$cluster

# Print the result
print(df_result)


cluster_assignments <- kmeans_result$cluster
dist_matrix <- dist(clustering_data)

library(dendextend)

dend <- as.dendrogram(hclust(dist_matrix))

# 可以根据需要调整绘图的样式和参数
plot(dend)


library(ggdendro)
library(ggplot2)

# 将dendrogram转换为dendro_data对象
dend_data <- dendro_data(dend)

# 创建颜色向量，用于给不同分组设置颜色
num_groups <- length(unique(dend_data$labels$label))
colors <- rainbow(num_groups)

# 设置绘图样式和标签
p <- ggplot() +
  geom_segment(data = dend_data$segments, aes(x = x, y = y, xend = xend, yend = yend),
               color = "gray") +
  geom_text(data = dend_data$labels, aes(x = x, y = y, label = label, color = label),
            hjust = -0.1, vjust = 0.5, size = 3) +
  scale_color_manual(values = colors) +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "right",
        plot.margin = margin(5, 5, 5, 10, "mm"))

print(p)




library(ggplot2)

# Create a ggplot object for the scatter plot
scatter_plot <- ggplot(df_result, aes(x = D7_pay_cnt, y = D7_sum_amount, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "K-Means Clustering Results",
       x = "pay_cnt",
       y = "sum_amount",
       color = "Cluster") +
  theme_minimal()

# Display the scatter plot
print(scatter_plot)


fviz_cluster(kmeans_result,df_result)

# Convert kmeans_result$centers to a dataframe
centers_df <- as.data.frame(kmeans_result$centers)


library(writexl)
write_xlsx(centers_df , "/Users/mazhecheng/Desktop/ifungames/20230809/centers_df.xlsx")

library(factoextra)
library(cluster)


library(cluster)
library(fpc)

# 调整的参数范围
k_values <- 2:30
nstart_values <- c(10, 25, 50,75,100)

# 存储评估指标
# Calculate silhouette scores manually without fpc package
silhouette_scores <- matrix(NA, nrow = length(k_values), ncol = length(nstart_values))

for (i in 1:length(k_values)) {
  for (j in 1:length(nstart_values)) {
    k <- k_values[i]
    nstart <- nstart_values[j]
    
    kmeans_result <- kmeans(clustering_data, centers = k, nstart = nstart)
    clusters <- kmeans_result$cluster
    distances <- dist(clustering_data)
    
    silhouette_scores[i, j] <- mean(silhouette(clusters, distances))
  }
}


print(silhouette_scores)

table(df_result$cluster)



library(ggplot2)

# 绘制直方图
ggplot(df_payment_rlvl, aes(x = pay_amount)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(title = "Pay Amount Histogram", x = "Pay Amount", y = "Frequency")


