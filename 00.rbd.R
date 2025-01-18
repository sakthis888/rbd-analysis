#load libraries
library(agricolae)
library(stats)
library(tidyverse)

#create a function
rbd_analysis  <- function(data, treatment,replication,mean_comparison_test,round_digits,parameter){
  
  rbd_analysis1 <- function(data, treatment,replication,mean_comparison_test,round_digits,parameter) {
    
    treatment <- as.factor(treatment)
    replication <- as.factor(replication)
    data = as.numeric(data)
    data_df = data.frame(treatment = treatment, replication = replication, data = data)
    
    #calculate ANOVA
    {
      anova_results <- anova(lm(data ~ treatment + replication))
      anova_df <- data.frame(
        Source = as.character(c("Treatment", "Replication", "Error")),
        df = as.character(anova_results[, 1]),
        SS = as.numeric(round(anova_results[, 2], 4)),
        MSS = as.numeric(round(anova_results[, 3], 4)),
        Ftab = as.numeric(round(anova_results[, 4], 4)),
        pval = as.numeric(round(anova_results[, 5], 4))
      )
      anova_df$Sig <- ifelse(is.na(anova_df$pval==""),"", ifelse(anova_df$pval < 0.05, "S", "NS"))
      colnames(anova_df)[5:7] <- c("F table", "p value","Significance (5%)")
      
      
      anova_df[] <- lapply(anova_df, function(x) {
        if (is.numeric(x)) {formatC(x, format = "f", digits = 4)} else {x} })
      anova_df$df <- round(as.numeric(anova_df$df),0)
      anova_df[3,5] <- ""
      anova_df[3,6] <- ""
    }
    
    #Summary
    {
      n_replications <- length(unique(replication))
      SEm <- round(sqrt(anova_results[3,3]/n_replications),round_digits)
      SEd <- round(sqrt(2)*SEm,round_digits)
      t_error_df <- round(qt(0.975, anova_results[3,1]),round_digits)
      CD <- round(SEd*t_error_df,round_digits)
      CV <- round((sqrt(as.numeric(anova_results[3, 3])) / mean(data) * 100),round_digits)
      
      # Create a summary dataframe
      summary_df <- data.frame(
        Statistic = c("SE (m)", "SE (d)", "t value", "CD (p<0.05)", "CV (%)"),
        Value = c(SEm, SEd, t_error_df, CD, CV))
      
      if (anova_results[1,5] > 0.05) {
        summary_df$Value[4] <- "NS"  # Set CD to "NS" if p-value > 0.05
      }
      
      # Round and format summary_df to specified digits
      summary_df[] <- lapply(summary_df, function(x) {
        if (is.numeric(x)) {
          x <- round(x, round_digits)  # Round first
          return(formatC(x, format = "f", digits = round_digits))  # Format to the specified decimal places
        } else {
          return(x)
        }
      })
      
      summary_df <-  as.data.frame(t(summary_df))
      colnames(summary_df) <- as.character(summary_df[1,])
      summary_df <- summary_df[-1, ]
    }
    
    #Multiple comparison
    {
      if (anova_results[1,5]>0.05){
        test1 <- "All the treatment means are same so dont go for any multiple comparison test"
      } else {
        if (mean_comparison_test == 0){
          test1 <- "No mean comparison test selected"
        } else {
          if (mean_comparison_test == 1){
            test <- LSD.test(data, treatment, anova_results[3, 1], anova_results[3, 3])
            test1 <- list(test$statistics, test$groups)
            test1 <- test1[[2]]
          } else 
            if (mean_comparison_test == 2){
              test <- duncan.test(data, treatment, anova_results[3, 1], anova_results[3, 3])
              test1 <- list(test$statistics, test$duncan, test$groups)
              test1 <- test1[[3]]
            } else 
              if (mean_comparison_test == 3){
                test <- HSD.test(data, treatment, anova_results[3, 1], anova_results[3, 3])
                test1 <- list(test$statistics, test$parameters, test$groups)
                test1 <- test1[[3]]
              }
          
          test1$Treatment <- rownames(test1)
          rownames(test1) <- NULL
          test1 <- test1[, c("Treatment", "data", "groups")]
          test1$data <- round(test1$data,round_digits)
          
          test1 <- test1 %>%
            mutate(TreatmentNumeric = ifelse(grepl("^T", Treatment), 
                                             as.numeric(gsub("T", "", Treatment)), 
                                             as.numeric(Treatment))) %>%  # Create numeric treatment column
            arrange(TreatmentNumeric) %>%  # Sort based on the numeric column
            select(-TreatmentNumeric)  # Remove the helper column after sorting
          
          test1[] <- lapply(test1, function(x) {
            if (is.numeric(x)) {
              x <- round(x, round_digits)  # Round first
              return(formatC(x, format = "f", digits = round_digits))  # Format to the specified decimal places
            } else {
              return(x)
            }
          })
        }
        
      }
    }
    
    # Grouping
    {
      if (is.character(test1)) {
        grouped_df = NULL
        
      } else {
        # Initialize an empty list to store treatments for each group
        grouped_list <- list()
        
        # Iterate through each row of the test1 dataframe
        for (i in 1:nrow(test1)) {
          # Split the group string into individual groups
          groups <- strsplit(as.character(test1$groups[i]), "")[[1]]
          
          # For each group in the split string, add the treatment to the corresponding group in grouped_list
          for (group in groups) {
            if (group %in% names(grouped_list)) {
              grouped_list[[group]] <- c(grouped_list[[group]], test1$Treatment[i])
            } else {
              grouped_list[[group]] <- test1$Treatment[i]
            }
          }
        }
        
        # Combine the treatments into a single string for each group
        grouped_df <- data.frame(
          Group = names(grouped_list),
          Treatments = sapply(grouped_list, function(treatments) paste(treatments, collapse = ", "))
        )  %>% arrange(Group)
      }
    }
    
    #Final Data
    {
      final_data <- data_df %>%
        group_by(Treatment = treatment) %>%
        summarise(mean = mean(data), median = median(data), sd = sd(data),se = (sd / sqrt(n())),
                  CI = (qt(0.975, df = n() - 1) * se), Q1 = quantile(data, probs = 0.25), Q3 = quantile(data, probs = 0.75),
                  IQR=IQR(data),min = min(data[data >= (Q1 - 1.5 *IQR)]),
                  max = max(data[data <= (Q3 + 1.5 *IQR)]))
      
      final_data <- final_data %>%
        mutate(TreatmentNumeric = ifelse(grepl("^T", Treatment), 
                                         as.numeric(gsub("T", "", Treatment)), 
                                         as.numeric(Treatment))) %>%  # Create numeric treatment column
        arrange(TreatmentNumeric) %>%  # Sort based on the numeric column
        select(-TreatmentNumeric)  # Remove the helper column after sorting
      
      create_final_new <- function(final_data, test1, parameter) {
        if (is.data.frame(test1) && "groups" %in% colnames(test1)) {
          final_data_with_groups <- final_data %>%
            left_join(test1 %>% select(Treatment, groups), by = "Treatment")
        } else {
          final_data_with_groups <- final_data %>%
            mutate(groups = "")
        }
        
        final_new <- switch(
          parameter,
          `1` = final_data_with_groups %>%
            mutate(Mean = formatC(mean, format = "f", digits = round_digits)) %>%
            select(Treatment, Mean),
          `2` = final_data_with_groups %>%
            mutate(`Mean` = paste0(formatC(mean, format = "f", digits = round_digits), groups)) %>%
            select(Treatment, `Mean`),
          `3` = final_data_with_groups %>%
            mutate(`Mean ± SE` = paste0(formatC(mean, format = "f", digits = round_digits), " ± ", formatC(se, format = "f", digits = round_digits))) %>%
            select(Treatment, `Mean ± SE`),
          `4` = final_data_with_groups %>%
            mutate(`Mean ± SD` = paste0(formatC(mean, format = "f", digits = round_digits), " ± ", formatC(sd, format = "f", digits = round_digits))) %>%
            select(Treatment, `Mean ± SD`),
          `5` = final_data_with_groups %>%
            mutate(`Mean ± CI` = paste0(formatC(mean, format = "f", digits = round_digits), " ± ", formatC(CI, format = "f", digits = round_digits))) %>%
            select(Treatment, `Mean ± CI`),
          `6` = final_data_with_groups %>%
            mutate(`Mean ± SE` = paste0(formatC(mean, format = "f", digits = round_digits), groups, " ± ", formatC(se, format = "f", digits = round_digits))) %>%
            select(Treatment, `Mean ± SE`),
          `7` = final_data_with_groups %>%
            mutate(`Mean ± SD` = paste0(formatC(mean, format = "f", digits = round_digits), groups, " ± ", formatC(sd, format = "f", digits = round_digits))) %>%
            select(Treatment, `Mean ± SD`),
          `8` = final_data_with_groups %>%
            mutate(`Mean ± CI` = paste0(formatC(mean, format = "f", digits = round_digits), groups, " ± ", formatC(CI, format = "f", digits = round_digits))) %>%
            select(Treatment, `Mean ± CI`),
          stop("Invalid parameter value. Must be an integer between 1 and 8.")
        )
        
        return(final_new)
      }
      
      final_new <- create_final_new(final_data, test1, parameter)
      raw_data <- data.frame(Treatment = c("SE (d)", "CD (p<0.05)"),  Value = c(summary_df[1,2],summary_df[1,4]))
      
      colnames(raw_data) <- colnames(final_new)
      final_new <-rbind(final_new,raw_data)
    }
    
    return(list(ANOVA = anova_df, Summary = summary_df,mct=test1,group=grouped_df,final_table=final_new))
  }  
  
  results <-  list()
  for (j in 1:ncol(data)){
    results[[j]]<-rbd_analysis1(data[[j]],treatment,replication,mean_comparison_test,round_digits,parameter)
  }
  names(results)<-names(data)
  return(results)
  
}

# df=read.csv("data.csv")
# rbd_analysis(df[4],df$TRT,df$REP,3,3,3)
