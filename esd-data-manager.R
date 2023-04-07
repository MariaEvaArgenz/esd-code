library("readxl")
library("dplyr")
library("tidyverse")
library("plyr")


data <- read_excel('/Users/David/Library/CloudStorage/OneDrive-UGent/Research Group Endoscopy/Data Collectors/ESD KPIs/ESD_Data validation.xlsx')

#remove cases where missing

#new

data_with_no_missing <- data

data_with_no_missing <- data_with_no_missing %>% select(c(2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 , 20, 21, 22))


data_with_no_missing$na_count <- apply(data_with_no_missing, 1, function(x) sum(is.na(x)))


data_with_no_missing <- data_with_no_missing %>% filter(!(na_count > 4))

respondent_ids <- data_with_no_missing$`adrema`

data_with_no_missing_final <- data %>%
  filter(`adrema` %in% respondent_ids)


data_with_no_missing_final <- data_with_no_missing_final %>% select(c(2:39))



mean(data_with_no_missing_final$r0, na.rm=TRUE)

group_by(data_with_no_missing_final, procedure_type_text) %>%
  summarise(
    count = n(),
    mean = mean(as.numeric(duration_text), na.rm = TRUE),
    sd = sd(as.numeric(duration_text), na.rm = TRUE),
    median = median(as.numeric(duration_text), na.rm = TRUE),
  )


table(data_with_no_missing_final$type_text, data_with_no_missing_final$procedure_type_text)
#todo add percentages



#how to add percentages to a column
tbl <- count(data_with_no_missing_final, 'en_bloc_text')
tbl['%'] <- 100*(tbl['freq']/sum(tbl['freq']))
print(tbl)




#calculations
data_with_no_missing_final$area <- data_with_no_missing_final$length_text * data_with_no_missing_final$width_text 

data_with_no_missing_final$speed <- data_with_no_missing_final$area / data_with_no_missing_final$duration_text

boxplot(data_with_no_missing_final$speed)
boxplot.stats(data_with_no_missing_final$speed)

boxplot(data_with_no_missing_final$area)
boxplot.stats(data_with_no_missing_final$area)



boxplot.stats(data_with_no_missing_final$r0)
boxplot(data_with_no_missing_final$r0)

#curative options

#if oesophagus squamous


getPercentages(data_with_no_missing_final, en_bloc_text)

getPercentages <- function(df, colName) {
  df.cnt <- df %>% select({{colName}}) %>% 
    table() %>%
    as.data.frame() %>% 
    rename({{colName}} :=1, Freq=2) %>% 
    mutate(Perc=100*Freq/sum(Freq))
}


r_zero <- function(hm, vm){
  
  if (missing('hm') == TRUE || missing('vm') == TRUE){
    
    return(NA)
    
  }
  
  if (hm == 0 && vm == 0) {
    
    r0 <- 1
    return(r0)
    
  }else{
    
    r0 <- 0
    return(r0)
    
  }
  
  
}

curative_enbloc <- function(hm, vm, smi, lvi, diff, depth, type){
  
  error <- qt(0.975,df=no_samples-1)*sd_sample/sqrt(no_samples)
  
  left_ci <- mean_sample - error
  right_ci <- mean_sample + error
  
  #confidence_interval <- list("lower bound" = left_ci, "mean" = mean_sample, "upper bound" = right_ci)
  confidence_interval <- c(left_ci, mean_sample,right_ci)
  
  return(confidence_interval)
  
}

for (i in data_with_no_missing_final$adrema){
  
  
  print('new line of dataframe')
  print(i)
  type <- data_with_no_missing_final$type[data_with_no_missing_final$adrema == i]
  # print(type)
  r0 <- data_with_no_missing_final$r0[data_with_no_missing_final$adrema == i]
  smi <- data_with_no_missing_final$smi[data_with_no_missing_final$adrema == i]
  lvi <- data_with_no_missing_final$lvi[data_with_no_missing_final$adrema == i]
  diff <- data_with_no_missing_final$diff[data_with_no_missing_final$adrema == i]
  depth <- data_with_no_missing_final$depth[data_with_no_missing_final$adrema == i]
  
  # print(smi)
  # print(r0)
  # print(lvi)
  # print(diff)
  
  print(curative(type, smi, r0, lvi, diff, depth))
  
}
  



curative <- function(type, smi, r0, lvi, diff, depth){
  
  print('new line of dataframe')
  print('type is')
  print(type)
  

    if (missing('type') == TRUE || missing('r0') == TRUE || missing('smi') == TRUE || missing('lvi') == TRUE || missing('diff') == TRUE) {
      return(NA)
    }

    if (type == 1) {
      if (smi == 0) {
        if (r0 == 1 && lvi == 0 && (diff == 2 || diff == 3)) {
          curative <- 1
          return(curative)
        } else {
          curative <- 0
          return(curative)
        }
      }
      if (smi == 1) {
        if (r0 == 1 && lvi == 0  && (diff == 2 || diff == 3) && depth == 1) {
          curative <- 1
          return(curative)
        } else {
          curative <- 0
          return(curative)
        }
      }
    }
  
  
  
  



   
   
  
}  

print(data_with_no_missing_final)

