# ===========================
# Figure 9: Paragraph-level sentiment in Chapter 5
# 版本：直接使用 bert_para.csv 中已有的 bert_score 和 lexicon_mean
# ===========================

pacman::p_load(tidyverse, readr)

# 1) 读取段落级结果 ----------------------------------
bert_df <- read_csv("bert_full.csv", show_col_types = FALSE)

# 如果列名和下面不完全一样，可以用 names(bert_df) 看一下，再改 rename
# 假定 bert_para.csv 至少有：
#   chapter, bert_score, lexicon_mean
# 如果章节列不是 chapter，例如 chap 或 chap_id：
if (!"chapter" %in% names(bert_df)) {
  cand <- names(bert_df)[grepl("^chap", tolower(names(bert_df)))]
  if (length(cand) >= 1) {
    bert_df <- bert_df %>% rename(chapter = !!cand[1])
  }
}

bert_df <- bert_df %>%
  mutate(chapter = as.integer(chapter))

# 2) 只保留第 5 章，并生成段落索引 --------------------
chap5 <- bert_df %>%
  filter(chapter == 5) %>%
  arrange(chapter) %>%
  mutate(para_index = row_number()) %>%
  # ⭐ 这里直接用文件里已有的 lexicon_mean，不再重新计算
  select(para_index, bert_score, lexicon_mean)

# 3) 转成长表，方便画两条线 --------------------------
chap5_long <- chap5 %>%
  pivot_longer(cols = c(bert_score, lexicon_mean),
               names_to = "method",
               values_to = "sentiment")

# 4) 绘图 ---------------------------------------------
fig9 <- ggplot(chap5_long,
               aes(x = para_index, y = sentiment,
                   colour = method, group = method)) +
  geom_line() +
  geom_point(size = 1) +
  scale_colour_manual(
    values = c("bert_score" = "#F8766D",
               "lexicon_mean" = "#00BFC4")
  ) +
  labs(
    title = "Paragraph-level sentiment in Chapter 5",
    x     = "Paragraph index",
    y     = "Sentiment score",
    colour = "Method"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(-1, 1))  # y 轴固定在 -1 到 1

ggsave("figure9_chap5_paragraph_sentiment.png",
       fig9, width = 7, height = 4, dpi = 300)

message("✅ Figure 9 已生成：figure9_chap5_paragraph_sentiment.png")


