# 清空当前环境
rm(list = ls())

# 加载路径
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# 加载必要的包
library(tidyverse)
library(ggpmisc)
library(broom)
library(ggpubr)
library(car)
library(randomForest)
library(mgcv)
library(gridExtra)
library(MASS)
library(lubridate)

# 加载数据
data <- read.csv("combined_stress_data.csv")
str(data)

# 转换数据类型
data$date <- as.Date(data$date, format = "%Y-%m-%d")
str(data)

# 添加季节变量
data$season <- case_when(
  month(data$date) %in% c(12, 1, 2) ~ "Winter",
  month(data$date) %in% c(3, 4, 5) ~ "Spring",
  month(data$date) %in% c(6, 7, 8) ~ "Summer",
  month(data$date) %in% c(9, 10, 11) ~ "Fall"
)

# 按季节和eventID计算每个变量的平均SE_score
seasonal_means <- data %>%
  group_by(eventID, season, variable) %>%
  summarise(mean_SE_score = mean(SE_score, na.rm = TRUE),
            mean_RESET_score = mean(RESET_score, na.rm = TRUE)) %>% 
  ungroup()
seasonal_means

# 计算总的Stressor和Reducer SE_scores，并计算RESET_score
total_mean_scores <- seasonal_means %>%
  group_by(eventID, variable) %>%
  summarise(SE_score = mean(mean_SE_score, na.rm = TRUE),
            RESET_score = mean(mean_RESET_score, na.rm = TRUE))
total_mean_scores

wide_data <- total_mean_scores %>%
  pivot_wider(names_from = variable, values_from = SE_score)

# evenID and sampleID
sam_even <- read.csv("sample_event_mapping.csv")

RESET_data <- wide_data %>% left_join(sam_even, by = "eventID")
RESET_data

Combined_Porifera_anthro <- read.csv("Combined_Porifera_anthro.csv")
Combined_Rhodophyta_anthro <- read.csv("Combined_Rhodophyta_anthro.csv")

Combined_Porifera_RESET <- RESET_data %>% left_join(Combined_Porifera_anthro, by = "SampleID") %>% 
  dplyr::select(2:7, 9, 11:15)
Combined_Rhodophyta_RESET <- RESET_data %>% left_join(Combined_Rhodophyta_anthro, by = "SampleID") %>% 
  dplyr::select(2:7, 9, 11:15)

colnames(Combined_Porifera_RESET)[c(4, 5)] <- c("SST_anomaly", "SST_variability")
colnames(Combined_Rhodophyta_RESET)[c(4, 5)] <- c("SST_anomaly", "SST_variability")

data = Combined_Porifera_RESET
response_variable = "Hill_q0"
species_name = "Porifera"
output_path = getwd()
k_values = 3:17


# 定义一个函数来处理分析过程
analyze_rf_lm_gam <- function(data, species_name, response_variable, output_path) {
  # 随机森林模型，仅使用独立的压力变量
  set.seed(111)
  
  clean_data <- na.omit(data)
  
  formula <- as.formula(paste(response_variable, "~ DHW + SST + SST_anomaly + SST_variability + 
                                        cloud + depth + wind"))
  rf_model <- randomForest(formula, data = clean_data, importance = TRUE, proximity = TRUE, na.action = na.omit)
  
  importance_scores <- importance(rf_model)[, 1]  # 提取重要性得分
  
  # 根据重要性得分降序排列特征
  importance_ordered <- sort(importance_scores, decreasing = TRUE)
  
  # 累积贡献率计算
  cumulative_importance <- cumsum(importance_ordered) / sum(importance_ordered)
  
  # 设置贡献率阈值（例如90%）
  threshold <- 0.9
  selected_features <- names(cumulative_importance)[cumulative_importance <= threshold]
  
  # 构建数据框
  importance_df <- data.frame(
    Feature = names(importance_ordered),
    Importance = importance_ordered,
    Cumulative_Importance = cumulative_importance
  )
  
  # 将随机森林的结果导出
  sink(file.path(output_path, paste("RESET_", "rf_", species_name, "_summary_", response_variable, ".txt", sep = "")))
  print("Randomforest Results:")
  print(importance_df)
  sink()
  
  # 可视化
  p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = 'identity', position = position_dodge(0.75), color = "black", fill = ifelse(importance_df$Cumulative_Importance <= threshold, "#FF7256", 'grey'), width = 0.75, size = 0.2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "black", size = 12, vjust = 0.5, hjust = 0.5),
          axis.text.y = element_text(color = "black", size = 12),
          legend.position = "none",
          panel.border = element_rect(color = "black", size = 0.5),
          axis.title = element_text(color = "black", size = 12),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'),
          strip.text = element_blank()) +
    coord_flip() +
    labs(x = "", y = "Feature importance (%)", title = response_variable)
  
  print(p)
  
  # 保存图像
  ggsave(filename = file.path(output_path, paste("RESET_", "rf_", species_name, "_", response_variable, ".pdf", sep = "")), plot = p)
  
  # 对挑选的变量进行单变量线性回归
  significant_vars <- c()  # 初始化存储显著变量的列表
  insignificant_vars <- c()  # 初始化存储不显著变量的列表
  plots <- list()  # 初始化存储图形的列表
  
  for (var in selected_features) {
    single_lm_formula <- as.formula(paste(response_variable, "~", var))
    single_lm_model <- lm(single_lm_formula, data = clean_data)
    
    p_value <- summary(single_lm_model)$coefficients[2, "Pr(>|t|)"]  # 获取解释变量的p值
    
    if (p_value <= 0.05) {
      significant_vars <- c(significant_vars, var)
      cat("Variable", var, "is significant with p-value =", p_value, "\n")
      
      # 生成单变量可视化图
      cor_test <- cor.test(clean_data[[var]], clean_data[[response_variable]], method = "pearson")
      cor_value <- cor_test$estimate
      smooth_color <- ifelse(cor_value > 0, "#E8541E", "#108EE9")
      smooth_line <- 1  # 显著性线型设为1
      
      p <- ggplot(clean_data, aes_string(x = var, y = response_variable)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE, color = smooth_color, fill = smooth_color,
                    lty = smooth_line, alpha = 0.2) +
        stat_poly_eq(use_label(c("R2", "P")), formula = y ~ x, parse = TRUE, size = 5) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text.x = element_text(color = "black", size = 12, vjust = 0.5, hjust = 0.5),
              axis.text.y = element_text(color = "black", size = 12),
              legend.position = "none",
              panel.border = element_rect(color = "black", size = 0.5),
              axis.title = element_text(color = "black", size = 12),
              axis.ticks.length.y = unit(0.1, 'cm'),
              axis.ticks.length.x = unit(0.1, 'cm'),
              strip.text = element_blank()) +
        labs(title = paste("Linear Regression of", response_variable, "vs", var),
             x = var, y = response_variable) 
      
      plots[[var]] <- p  # 存储图形
      
    } else {
      insignificant_vars <- c(insignificant_vars, var)
      cat("Variable", var, "is not significant with p-value =", p_value, "\n")
    }
  }
  
  # 在循环外，可以根据significant_vars生成最终的图形列表
  final_plots <- list()
  for (var in significant_vars) {
    final_plots[[var]] <- plots[[var]]
  }
  
  # 将单变量可视化图输出到PDF
  if (length(final_plots) > 0) {
    pdf(file.path(output_path, paste("Hill_Diversity_vs_RESET_SingleVar_LM_", response_variable, "_", species_name, ".pdf", sep = "")), height = 6, width = 10)
    do.call(grid.arrange, c(final_plots, ncol = 2))
    dev.off()
  } else {
    cat("No plots generated for single variable linear regressions.\n")
  }
  
  # 如果有显著的变量，则构建多元线性回归模型
  if (length(significant_vars) > 0) {
    formula_string <- paste(response_variable, "~", paste(significant_vars, collapse = " + "))
    lm_formula <- as.formula(formula_string)
    lm_model <- lm(lm_formula, data = clean_data)
    print(summary(lm_model))
    
    # 保存最初的lm模型
    original_lm_model <- lm_model
    
    # Stepwise逐步回归 (采用AIC标准)
    step_model <- stepAIC(lm_model, direction = "both", trace = FALSE)
    
    # 剔除coef为NA的变量
    na_vars <- names(coef(lm_model))[is.na(coef(lm_model))]
    if (length(na_vars) > 0) {
      print(paste("Variables with NA coefficients:", paste(na_vars, collapse = ", ")))
      
      # 检查这些变量的数据情况
      for (var in na_vars) {
        print(paste("Summary of", var))
        print(summary(clean_data[[var]]))
        print(paste("Number of missing values in", var, ":", sum(is.na(clean_data[[var]]))))
      }
      
      # 考虑移除NA系数的变量重新拟合模型
      lm_formula <- update(lm_formula, paste(". ~ . -", paste(na_vars, collapse = " - ")))
      lm_model <- lm(lm_formula, data = clean_data)
      print("Updated model after removing NA variables:")
      print(summary(lm_model))
    }
    
    # 重新拟合模型后再计算VIF,检查多重共线性
    while (TRUE) {
      if (length(coef(lm_model)) < 2) {
        cat("模型中变量不足以计算VIF。\n")
        break
      }
      
      vif_values <- try(vif(lm_model), silent = TRUE)
      
      if (inherits(vif_values, "try-error")) {
        cat("VIF calculation failed due to NA values or other issues.\n")
        break
      } else {
        max_vif_var <- names(which.max(vif_values))
        max_vif_value <- max(vif_values)
        
        if (max_vif_value > 10) {
          cat("Variable", max_vif_var, "has VIF =", max_vif_value, ". Removing this variable and re-fitting the model.\n")
          lm_formula <- update(lm_formula, paste(". ~ . -", max_vif_var))
          lm_model <- lm(lm_formula, data = clean_data)
        } else {
          cat("No VIF values greater than 10. Stopping VIF check.\n")
          break
        }
      }
    }
    
    # 打印lm的结果
    sink(file.path(output_path, paste("RESET_", "lm_", species_name, "_summary_", response_variable, ".txt", sep = "")))
    print("Initial Model Summary:")
    print(summary(original_lm_model))
    print("Model After Stepwise Selection:")
    print(summary(step_model))
    print("VIF value:")
    print(vif_values)
    sink()
    
  } else {
    cat("No significant variables found for multiple linear regression.\n")
  }
  
  # 处理不显著的变量：这些变量将在后续的GAM模型中使用
  if (length(insignificant_vars) > 0) {
    cat("The following variables will be used in the GAM model:", paste(insignificant_vars, collapse = ", "), "\n")
  } else {
    cat("No insignificant variables to use in the GAM model.\n")
  }
  
  # 检查insignificant_vars是否为空
  if (length(insignificant_vars) == 0) {
    cat("所有变量在线性模型中均显著，跳过GAM模型构建。\n")
    gam_model <- NULL  # 确保gam_model为空，以防后续处理
  } else {
    # 如果存在不显著的变量，则继续GAM模型构建
    best_k_results <- lapply(insignificant_vars, function(var) {
      tryCatch({
        aic_values <- numeric(length(k_values))
        for (i in seq_along(k_values)) {
          k <- k_values[i]
          gam_formula <- as.formula(paste(response_variable, "~ s(", var, ", k =", k, ")"))
          gam_model <- gam(gam_formula, data = clean_data)
          aic_values[i] <- AIC(gam_model)
        }
        best_k <- k_values[which.min(aic_values)]
        list(variable = var, best_k = best_k, aic_values = aic_values)
      }, error = function(e) {
        NULL
      })
    })
    
    # 过滤出有效的k值结果
    valid_results <- Filter(Negate(is.null), best_k_results)
    
    if (length(valid_results) > 0) {
      factor_vars <- names(clean_data)[sapply(clean_data, function(x) is.factor(x) && var(x) > 0)]
      smooth_terms <- paste(sapply(valid_results, function(res) {
        paste("s(", res$variable, ", k =", res$best_k, ")")
      }), collapse = " + ")
      
      gam_formula <- if (length(factor_vars) > 0) {
        as.formula(paste(response_variable, "~", paste(factor_vars, collapse = " + "), "+", smooth_terms))
      } else {
        as.formula(paste(response_variable, "~", smooth_terms))
      }
      
      gam_model <- gam(gam_formula, data = clean_data, select = TRUE)
      summary(gam_model)
      if (length(valid_results) == 1) {
        remaining_vars <- valid_results
        cat("Only one significant variable found. Skipping stepwise selection.\n")
      } else {
        remaining_vars <- valid_results
        repeat {
          p_values <- summary(gam_model)$s.table[, "p-value"]
          max_p_value <- max(p_values, na.rm = TRUE)
          max_p_var <- rownames(summary(gam_model)$s.table)[which.max(p_values)]
          cleaned_var <- gsub("s\\(|\\)", "", max_p_var)
          # 提取所有变量名
          variable_names <- sapply(remaining_vars, function(item) item[["variable"]])
          
          if (max_p_value > 0.05) {
            cat("Variable", max_p_var, "has p-value =", max_p_value, ". Removing this variable and re-fitting the model.\n")
            old_gam_formula <- gam_formula
            gam_formula <- update(old_gam_formula, paste(". ~ . - s(", max_p_var, ")"))
            remaining_vars <- remaining_vars[variable_names != cleaned_var]
            
            if (identical(old_gam_formula, gam_formula)) {
              cat("Formula update failed or variable was not correctly removed. Stopping stepwise selection.\n")
              break
            }
            
            gam_model <- gam(gam_formula, data = clean_data)
          } else {
            cat("No p-values greater than 0.05. Stopping stepwise selection.\n")
            break
          }
        }
      }
      
      if(length(remaining_vars) > 0){
        # 使用 paste 函数构建公式，s() 用于表示样条平滑函数
        new_smooth_terms <- paste(sapply(remaining_vars, function(res) {
          paste("s(", remaining_vars[[1]][["variable"]], ", k =", remaining_vars[[1]][["best_k"]], ")")
        }), collapse = " + ")
        
        new_formula <- as.formula(paste(response_variable, "~", 
                                        paste(factor_vars, collapse = " + "), "+", 
                                        new_smooth_terms))
        
        # 拟合新的 GAM 模型
        # 假设 clean_data 是你的数据集
        new_gam_model <- gam(new_formula, data = clean_data)
        
        # 打印新模型的摘要
        summary(new_gam_model)
      }
      
      # 打印并保存GAM模型结果
      sink(file.path(output_path, paste("RESET_", "gam_", species_name, "_summary_", response_variable, ".txt", sep = "")))
      print("GAM best k of variables:")
      print(remaining_vars)
      print("GAM Model Summary:")
      print(summary(gam_model))
      print("GAM NEW Model Summary:")
      print(summary(new_gam_model))
      sink()
      
    } else {
      cat("未找到有效的k值或无连续变量进行平滑处理。\n")
    }
  }
  
  # 绘制GAM图
  if (!is.null(gam_model) && length(remaining_vars) > 0) {
    gam_plots <- lapply(remaining_vars, function(res) {
      ggplot(clean_data, aes_string(x = res$variable, y = response_variable)) +
        geom_point() +
        geom_smooth(method = "gam", formula = y ~ s(x, k = res$best_k), col = "red") +
        labs(title = paste("GAM for", response_variable, "with", res$variable),
             x = res$variable, y = response_variable) +
        theme_minimal()
    })
    
    if (length(gam_plots) > 0) {
      pdf(file.path(output_path, paste("Hill_Diversity_vs_RESET_GAM_", response_variable, "_", species_name, ".pdf", sep = "")), height = 6, width = 12)
      do.call(grid.arrange, c(gam_plots, ncol = 3))
      dev.off()
    } else {
      cat("没有有效的绘图对象，跳过GAM图生成。\n")
    }
  } else {
    cat("GAM模型未构建或无效，跳过GAM图生成。\n")
  }
  
  return(list(
    lm_summary = if (exists("lm_model")) summary(lm_model) else NULL,
    gam_summary = if (!is.null(gam_model)) summary(gam_model) else NULL
  ))
}

# 函数所需参数
species_list = list(Porifera = Combined_Porifera_RESET, 
                    Rhodophyta = Combined_Rhodophyta_RESET)

response_variable = c("Hill_q0", "Hill_q1", "Hill_q2")

output_path <- getwd()

k_values = 3:17

# 循环分析每个物种和因变量，并生成合并的图表

for (species_name in names(species_list)) {
  for (response_var in response_variable) {
    # 调用分析函数，并获取结果
    labeled_var <- analyze_rf_lm_gam(species_list[[species_name]], species_name, response_var, output_path)
    
    # 如果分析函数返回结果，将其合并到总数据框中
    # if (!is.null(labeled_var)) {
    #   total <- rbind(total, labeled_var)
    # }
  }
}



# RESET_score #### 
data = Combined_Rhodophyta_anthro
response_variable = "Hill_q2"
species_name = "Rhodophyta"
output_path = getwd()
k_values = 3:17

# 定义一个函数来处理分析过程
analyze_rf_lm_gam <- function(data, species_name, response_variable, output_path) {
  # 随机森林模型，仅使用独立的压力变量
  set.seed(111)
  
  clean_data <- na.omit(data)
  
  formula <- as.formula(paste(response_variable, "~ RESET_score"))
  rf_model <- randomForest(formula, data = clean_data, importance = TRUE, 
                           proximity = TRUE, na.action = na.omit)
  
  importance_scores <- importance(rf_model)[, 1]  # 提取重要性得分
  
  # 根据重要性得分降序排列特征
  importance_ordered <- sort(importance_scores, decreasing = TRUE)
  
  # 累积贡献率计算
  cumulative_importance <- cumsum(importance_ordered) / sum(importance_ordered)
  
  selected_features <- "RESET_score"
  
  # 构建数据框
  importance_df <- data.frame(
    Feature = "RESET_score",
    Importance = importance_ordered,
    Cumulative_Importance = cumulative_importance
  )
  
  # 将随机森林的结果导出
  sink(file.path(output_path, paste("RESET_", "_RESET_score_", "rf_", species_name, "_summary_", response_variable, ".txt", sep = "")))
  print("Randomforest Results:")
  print(importance_df)
  sink()
  
  # 可视化
  p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = 'identity', position = position_dodge(0.75), 
             color = "black", 
             fill = "#FF7256", 
             width = 0.5, size = 0.2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "black", size = 12, vjust = 0.5, hjust = 0.5),
          axis.text.y = element_text(color = "black", size = 12),
          legend.position = "none",
          panel.border = element_rect(color = "black", size = 0.5),
          axis.title = element_text(color = "black", size = 12),
          axis.ticks.length.y = unit(0.1, 'cm'),
          axis.ticks.length.x = unit(0.1, 'cm'),
          strip.text = element_blank()) +
    coord_flip() +
    labs(x = "", y = "Feature importance (%)", title = response_variable)
  
  print(p)
  
  # 保存图像
  ggsave(filename = file.path(output_path, paste("RESET_", "_RESET_score_", "rf_", species_name, "_", response_variable, ".pdf", sep = "")), plot = p)
  
  # 对挑选的变量进行单变量线性回归
  significant_vars <- c()  # 初始化存储显著变量的列表
  insignificant_vars <- c()  # 初始化存储不显著变量的列表
  plots <- list()  # 初始化存储图形的列表
  
  for (var in selected_features) {
    single_lm_formula <- as.formula(paste(response_variable, "~", var))
    single_lm_model <- lm(single_lm_formula, data = clean_data)
    
    p_value <- summary(single_lm_model)$coefficients[2, "Pr(>|t|)"]  # 获取解释变量的p值
    
    if (p_value <= 0.05) {
      significant_vars <- c(significant_vars, var)
      cat("Variable", var, "is significant with p-value =", p_value, "\n")
      
      # 生成单变量可视化图
      cor_test <- cor.test(clean_data[[var]], clean_data[[response_variable]], method = "pearson")
      cor_value <- cor_test$estimate
      smooth_color <- ifelse(cor_value > 0, "#E8541E", "#108EE9")
      smooth_line <- 1  # 显著性线型设为1
      
      p <- ggplot(clean_data, aes_string(x = var, y = response_variable)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE, color = smooth_color, fill = smooth_color,
                    lty = smooth_line, alpha = 0.2) +
        stat_poly_eq(use_label(c("R2", "P")), formula = y ~ x, parse = TRUE, size = 5) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text.x = element_text(color = "black", size = 12, vjust = 0.5, hjust = 0.5),
              axis.text.y = element_text(color = "black", size = 12),
              legend.position = "none",
              panel.border = element_rect(color = "black", size = 0.5),
              axis.title = element_text(color = "black", size = 12),
              axis.ticks.length.y = unit(0.1, 'cm'),
              axis.ticks.length.x = unit(0.1, 'cm'),
              strip.text = element_blank()) +
        labs(title = paste("Linear Regression of", response_variable, "vs", var),
             x = var, y = response_variable) 
      
      plots[[var]] <- p  # 存储图形
      
    } else {
      insignificant_vars <- c(insignificant_vars, var)
      cat("Variable", var, "is not significant with p-value =", p_value, "\n")
    }
  }
  
  # 在循环外，可以根据significant_vars生成最终的图形列表
  final_plots <- list()
  for (var in significant_vars) {
    final_plots[[var]] <- plots[[var]]
  }
  
  # 将单变量可视化图输出到PDF
  if (length(final_plots) > 0) {
    pdf(file.path(output_path, paste("Hill_Diversity_vs_RESET_SingleVar_LM_RESET_score_", response_variable, "_", species_name, ".pdf", sep = "")), height = 6, width = 10)
    do.call(grid.arrange, c(final_plots, ncol = 2))
    dev.off()
  } else {
    cat("No plots generated for single variable linear regressions.\n")
  }
  
  # 如果有显著的变量，则构建多元线性回归模型
  if (length(significant_vars) > 0) {
    formula_string <- paste(response_variable, "~", paste(significant_vars, collapse = " + "))
    lm_formula <- as.formula(formula_string)
    lm_model <- lm(lm_formula, data = clean_data)
    print(summary(lm_model))
    
    # 保存最初的lm模型
    original_lm_model <- lm_model
    
    # Stepwise逐步回归 (采用AIC标准)
    step_model <- stepAIC(lm_model, direction = "both", trace = FALSE)
    
    # 剔除coef为NA的变量
    na_vars <- names(coef(lm_model))[is.na(coef(lm_model))]
    if (length(na_vars) > 0) {
      print(paste("Variables with NA coefficients:", paste(na_vars, collapse = ", ")))
      
      # 检查这些变量的数据情况
      for (var in na_vars) {
        print(paste("Summary of", var))
        print(summary(clean_data[[var]]))
        print(paste("Number of missing values in", var, ":", sum(is.na(clean_data[[var]]))))
      }
      
      # 考虑移除NA系数的变量重新拟合模型
      lm_formula <- update(lm_formula, paste(". ~ . -", paste(na_vars, collapse = " - ")))
      lm_model <- lm(lm_formula, data = clean_data)
      print("Updated model after removing NA variables:")
      print(summary(lm_model))
    }
    
    # 重新拟合模型后再计算VIF,检查多重共线性
    while (TRUE) {
      if (length(coef(lm_model)) < 2) {
        cat("模型中变量不足以计算VIF。\n")
        break
      }
      
      vif_values <- try(vif(lm_model), silent = TRUE)
      
      if (inherits(vif_values, "try-error")) {
        cat("VIF calculation failed due to NA values or other issues.\n")
        break
      } else {
        max_vif_var <- names(which.max(vif_values))
        max_vif_value <- max(vif_values)
        
        if (max_vif_value > 10) {
          cat("Variable", max_vif_var, "has VIF =", max_vif_value, ". Removing this variable and re-fitting the model.\n")
          lm_formula <- update(lm_formula, paste(". ~ . -", max_vif_var))
          lm_model <- lm(lm_formula, data = clean_data)
        } else {
          cat("No VIF values greater than 10. Stopping VIF check.\n")
          break
        }
      }
    }
    
    # 打印lm的结果
    sink(file.path(output_path, paste("RESET_", "_RESET_score_", "lm_", species_name, "_summary_", response_variable, ".txt", sep = "")))
    print("Initial Model Summary:")
    print(summary(original_lm_model))
    print("Model After Stepwise Selection:")
    print(summary(step_model))
    print("VIF value:")
    print(vif_values)
    sink()
    
  } else {
    cat("No significant variables found for multiple linear regression.\n")
  }
  
  # 处理不显著的变量：这些变量将在后续的GAM模型中使用
  if (length(insignificant_vars) > 0) {
    cat("The following variables will be used in the GAM model:", paste(insignificant_vars, collapse = ", "), "\n")
  } else {
    cat("No insignificant variables to use in the GAM model.\n")
  }
  
  # 检查insignificant_vars是否为空
  if (length(insignificant_vars) == 0) {
    cat("所有变量在线性模型中均显著，跳过GAM模型构建。\n")
    gam_model <- NULL  # 确保gam_model为空，以防后续处理
  } else {
    # 如果存在不显著的变量，则继续GAM模型构建
    best_k_results <- lapply(insignificant_vars, function(var) {
      tryCatch({
        aic_values <- numeric(length(k_values))
        for (i in seq_along(k_values)) {
          k <- k_values[i]
          gam_formula <- as.formula(paste(response_variable, "~ s(", var, ", k =", k, ")"))
          gam_model <- gam(gam_formula, data = clean_data)
          aic_values[i] <- AIC(gam_model)
        }
        best_k <- k_values[which.min(aic_values)]
        list(variable = var, best_k = best_k, aic_values = aic_values)
      }, error = function(e) {
        NULL
      })
    })
    
    # 过滤出有效的k值结果
    valid_results <- Filter(Negate(is.null), best_k_results)
    
    if (length(valid_results) > 0) {
      factor_vars <- names(clean_data)[sapply(clean_data, function(x) is.factor(x) && var(x) > 0)]
      smooth_terms <- paste(sapply(valid_results, function(res) {
        paste("s(", res$variable, ", k =", res$best_k, ")")
      }), collapse = " + ")
      
      gam_formula <- if (length(factor_vars) > 0) {
        as.formula(paste(response_variable, "~", paste(factor_vars, collapse = " + "), "+", smooth_terms))
      } else {
        as.formula(paste(response_variable, "~", smooth_terms))
      }
      
      gam_model <- gam(gam_formula, data = clean_data, select = TRUE)
      summary(gam_model)
      if (length(valid_results) == 1) {
        remaining_vars <- valid_results
        cat("Only one significant variable found. Skipping stepwise selection.\n")
      } else {
        remaining_vars <- valid_results
        repeat {
          p_values <- summary(gam_model)$s.table[, "p-value"]
          max_p_value <- max(p_values, na.rm = TRUE)
          max_p_var <- rownames(summary(gam_model)$s.table)[which.max(p_values)]
          cleaned_var <- gsub("s\\(|\\)", "", max_p_var)
          # 提取所有变量名
          variable_names <- sapply(remaining_vars, function(item) item[["variable"]])
          
          if (max_p_value > 0.05) {
            cat("Variable", max_p_var, "has p-value =", max_p_value, ". Removing this variable and re-fitting the model.\n")
            old_gam_formula <- gam_formula
            gam_formula <- update(old_gam_formula, paste(". ~ . - s(", max_p_var, ")"))
            remaining_vars <- remaining_vars[variable_names != cleaned_var]
            
            if (identical(old_gam_formula, gam_formula)) {
              cat("Formula update failed or variable was not correctly removed. Stopping stepwise selection.\n")
              break
            }
            
            gam_model <- gam(gam_formula, data = clean_data)
          } else {
            cat("No p-values greater than 0.05. Stopping stepwise selection.\n")
            break
          }
        }
      }
      
      if(length(remaining_vars) > 0){
        # 使用 paste 函数构建公式，s() 用于表示样条平滑函数
        new_smooth_terms <- paste(sapply(remaining_vars, function(res) {
          paste("s(", remaining_vars[[1]][["variable"]], ", k =", remaining_vars[[1]][["best_k"]], ")")
        }), collapse = " + ")
        
        new_formula <- as.formula(paste(response_variable, "~", 
                                        paste(factor_vars, collapse = " + "), "+", 
                                        new_smooth_terms))
        
        # 拟合新的 GAM 模型
        # 假设 clean_data 是你的数据集
        new_gam_model <- gam(new_formula, data = clean_data)
        
        # 打印新模型的摘要
        summary(new_gam_model)
      }
      
      # 打印并保存GAM模型结果
      sink(file.path(output_path, paste("RESET_", "_RESET_score_", "gam_", species_name, "_summary_", response_variable, ".txt", sep = "")))
      print("GAM best k of variables:")
      print(remaining_vars)
      print("GAM Model Summary:")
      print(summary(gam_model))
      print("GAM NEW Model Summary:")
      print(summary(new_gam_model))
      sink()
      
    } else {
      cat("未找到有效的k值或无连续变量进行平滑处理。\n")
    }
  }
  
  # 绘制GAM图
  if (!is.null(gam_model) && length(remaining_vars) > 0) {
    gam_plots <- lapply(remaining_vars, function(res) {
      ggplot(clean_data, aes_string(x = res$variable, y = response_variable)) +
        geom_point() +
        geom_smooth(method = "gam", formula = y ~ s(x, k = res$best_k), col = "red") +
        labs(title = paste("GAM for", response_variable, "with", res$variable),
             x = res$variable, y = response_variable) +
        theme_minimal()
    })
    
    if (length(gam_plots) > 0) {
      pdf(file.path(output_path, paste("Hill_Diversity_vs_RESET_GAM_RESET_score", response_variable, "_", species_name, ".pdf", sep = "")), height = 6, width = 12)
      do.call(grid.arrange, c(gam_plots, ncol = 3))
      dev.off()
    } else {
      cat("没有有效的绘图对象，跳过GAM图生成。\n")
    }
  } else {
    cat("GAM模型未构建或无效，跳过GAM图生成。\n")
  }
  
  return(list(
    lm_summary = if (exists("lm_model")) summary(lm_model) else NULL,
    gam_summary = if (!is.null(gam_model)) summary(gam_model) else NULL
  ))
}

# 函数所需参数
species_list = list(Porifera = Combined_Porifera_RESET, 
                    Rhodophyta = Combined_Rhodophyta_RESET)

response_variable = c("Hill_q0", "Hill_q1", "Hill_q2")

output_path <- getwd()

k_values = 3:17

# 循环分析每个物种和因变量，并生成合并的图表

for (species_name in names(species_list)) {
  for (response_var in response_variable) {
    # 调用分析函数，并获取结果
    labeled_var <- analyze_rf_lm_gam(species_list[[species_name]], species_name, response_var, output_path)
  }
}

