## Clustering with td simulated data
## number of cluster argument to be supplied
Clust_Data <- function(x) {
  
  clust_arg = x
  library(e1071)
  library(dplyr)
  library(magrittr)
  df <- read.csv("td_simul_small.csv", header=TRUE)
  df_sum <- df %>% 
    group_by(RRcode) %>%
    summarise(avg_nag= mean(nag), 
              avg_depletion = mean(depletion),
              avg_production = mean(production),
              min_nag = min(nag),
              min_depletion = min(depletion),
              min_production = min(production),
              max_nag = max(nag),
              max_depletion = max(depletion),
              max_production = max(production))
  
  clust_model <- kmeans(df_sum[,2:10], clust_arg)
  
  df_sum$cluster <- clust_model$cluster
  
  ##write.csv(df_sum, file = "simulated_withCluster.csv")
  
  df_sum_summary <- df_sum %>%
    group_by(cluster) %>%
    summarise( avg_nag= mean(avg_nag), 
               avg_depletion = mean(avg_depletion),
               avg_production = mean(avg_production),
               min_nag = min(min_nag),
               min_depletion = min(min_depletion),
               min_production = min(min_production),
               max_nag = max(max_nag),
               max_depletion = max(max_depletion),
               max_production = max(max_production),
               numAgents = n())
  
  return (paste(df_sum_summary$avg_depletion,df_sum_summary$avg_production,df_sum_summary$numAgents,sep = "~"))
}


