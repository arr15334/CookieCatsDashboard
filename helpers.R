ds <- read.csv('../cookie_cats.csv')

A = 'gate_30'
B = 'gate_40'

# we can immediatly see there is one strong outlier 
# playing the game unreasonably long
# we find and exclude it from the analysis
summary(ds)
id = ds$userid[ds$sum_gamerounds>4000]
ds = ds[ds$userid!=id,]


AB_result <- function(df,weight1,weight2,weight3,wilcox=TRUE,
                       feature='ALL'){
  # new column with weighted sum
  max_gamerounds <- max(df$sum_gamerounds)
  min_gamerounds <- min(df$sum_gamerounds)
  
  df['weighted_value'] = weight1 * (df$sum_gamerounds-min_gamerounds)/(max_gamerounds-min_gamerounds) +
    weight2 * df[,4] + weight3 * df[,5]
  
  # split according to A/B groups and selected feature
  if (feature == 'ALL') {
    control   = df['weighted_value'][df['version']==A]
    treatment = df['weighted_value'][df['version']==B]
  } else if (toupper(feature) == 'ONEDAY') {
    control   = df['retention_1'][df['version']==A]
    treatment = df['retention_1'][df['version']==B]
  } else if (toupper(feature) == 'SEVENDAY') {
    control   = df['retention_7'][df['version']==A]
    treatment = df['retention_7'][df['version']==B]
  } else {
    control   = df['sum_gamerounds'][df['version']==A]
    treatment = df['sum_gamerounds'][df['version']==B]
  }
  
  if (wilcox){
    test = wilcox.test(control,treatment,conf.int=TRUE)
  }
  else{
    test = prop.test(x = c(sum(control), sum(treatment)),
                     n =c(length(control), length(treatment)))
  }
  
  return(test$p.value)
  # or:
  #return(test) # and afterwards access test$p-value test$conf.int
}

get_retention_df <- function(oneday=T) {
  if (oneday) {
    control   = ds['retention_1'][ds['version']==A]
    treatment = ds['retention_1'][ds['version']==B]
  } else {
    control   = ds['retention_7'][ds['version']==A]
    treatment = ds['retention_7'][ds['version']==B]
  }
    p_df <- data.frame(list(group=c('Control', 'Control', 'Treatment', 'Treatment'),
                            retention=c('Yes', 'No', 'Yes', 'No'),
                            value=c(0, 0, 0, 0)))
    p_df[1,3] <- round(sum(control) / length(control)*100,2)
    p_df[2,3] <- 100 - p_df[1,3]
    p_df[3,3] <- round(sum(treatment) / length(treatment)*100,2)
    p_df[4,3] <- 100 - p_df[3,3]
    return(p_df)
}

get_groups = function(df,col_name){
  group_A = df[df['version']==A][col_name]
  group_B = df[df['version']==B][col_name]
  return(c(group_A,group_B))
}




get_uplift = function(df,retention){
  ret1_A = ds[retention][ds['version']=='gate_30']
  ret1_B = ds[retention][ds['version']=='gate_40']
  
  conv_rate_A = sum(ret1_A)/ length(ret1_A)
  conv_rate_B = sum(ret1_B) / length(ret1_B)
  
  uplift <- (conv_rate_B - conv_rate_A)/ conv_rate_A * 100
  return(uplift) 
}

retention_games <- function(maxGames) {
  x <- maxGames
  y <- vector()
  for (i in 1:x) {
    y[i] <- mean(ds$retention_7[ds$sum_gamerounds > i])
  }
  return(y)
}

