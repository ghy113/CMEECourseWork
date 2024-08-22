# 加载必要的包
library(phyloseq)
library(dplyr)


ps <- readRDS("ps_COI_sessile_cOTUs_clean_Ransome.RDS")
# 提取 OTU 表和 tax_table
otu <- otu_table(ps)
tax <- tax_table(ps)

# 确保 OTU 表是一个矩阵
otu <- as.matrix(otu)

# 筛选出 Rank_3 为 Porifera 的 OTU
porifera_otu <- otu[which(tax[, "Rank_3"] == "Porifera"), ]

# 筛选出 Rank_3 为 Rhodophyta 的 OTU
rhodophyta_otu <- otu[which(tax[, "Rank_3"] == "Rhodophyta"), ]

# 计算总计数和相对丰度
calculate_relative_abundance <- function(otu_data) {
  total_counts <- colSums(otu_data)
  rel_abundance <- sweep(otu_data, 2, total_counts, "/")
  return(rel_abundance)
}

rel_abundance_porifera <- calculate_relative_abundance(porifera_otu)
rel_abundance_rhodophyta <- calculate_relative_abundance(rhodophyta_otu)

# 定义 Shannon 熵和 Hill 数（保持不变）
shannon_entropy <- function(p) {
  p <- p[p > 0]
  -sum(p * log(p))
}

hill_number <- function(p, q) {
  if (q == 1) {
    return(exp(shannon_entropy(p)))
  } else if (q == 0) {
    return(sum(p > 0))
  } else {
    return((sum(p^q))^(1/(1-q)))
  }
}

q_values <- c(0, 1, 2)

# 计算每个样本的 Hill 数量
calculate_hill_numbers <- function(rel_abundance) {
  apply(rel_abundance, 2, function(p) {
    sapply(q_values, function(q) hill_number(p, q))
  })
}

hill_numbers_porifera <- calculate_hill_numbers(rel_abundance_porifera)
hill_numbers_rhodophyta <- calculate_hill_numbers(rel_abundance_rhodophyta)

# 转换为数据框并添加样本 ID 列
create_hill_dataframe <- function(hill_numbers, ps, group_name) {
  hill_df <- as.data.frame(t(hill_numbers))
  colnames(hill_df) <- paste0("Hill_q", q_values)
  hill_df$SampleID <- sample_names(ps)
  hill_df$Group <- group_name
  return(hill_df)
}

hill_Porifera <- create_hill_dataframe(hill_numbers_porifera, ps, "Porifera")
hill_Rhodophyta <- create_hill_dataframe(hill_numbers_rhodophyta, ps, "Rhodophyta")



