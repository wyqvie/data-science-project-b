pacman::p_load(tidyverse, readr, syuzhet, sentimentr, ggcorrplot)

# 1) 读入每章文本
chap <- read_csv("around_world_80_days_clean_v2.csv", show_col_types = FALSE)

# 2) 词典法情感（每章一个值）
lexicon_df <- chap %>%
  mutate(
    nrc_sent   = get_sentiment(text, method = "syuzhet"),  # NRC
    bing_sent  = get_sentiment(text, method = "bing"),     # Bing
    afinn_sent = get_sentiment(text, method = "afinn")     # AFINN
  ) %>%
  select(chapter, nrc_sent, bing_sent, afinn_sent)

# 3) sentimentr：句子级 → 每章平均值（每章1个数字）
sentimentr_df <- chap %>%
  select(chapter, text) %>%
  mutate(
    sentimentr_sent = purrr::map_dbl(text, ~ {
      s <- sentimentr::sentiment(.x)
      mean(s$sentiment, na.rm = TRUE)
    })
  )

# 4) 合并四种方法
sent_all <- lexicon_df %>%
  left_join(sentimentr_df, by = "chapter") %>%
  # 确保列顺序是：nrc → bing → afinn → sentimentr（和你原来的图一致）
  select(nrc_sent, bing_sent, afinn_sent, sentimentr_sent)

# 5) 计算 4×4 相关系数矩阵
corr_mat <- cor(sent_all, use = "pairwise.complete.obs", method = "pearson")

# 6) 画完整矩阵
p4 <- ggcorrplot(
  corr_mat,
  type = "full",          
  lab  = TRUE,
  lab_size = 4,
  outline.col = "white",
  colors = c("#B2182B", "white", "#4D4D8C"),  
  hc.order = FALSE         
) +
  ggtitle("Figure 4. Correlation among sentiment methods") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# 7) 保存图片
ggsave("figure4_sentiment_correlation.png", p4,
       width = 10, height = 6, dpi = 300)

