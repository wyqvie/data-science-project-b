pacman::p_load(tidyverse, readr, syuzhet, purrr, stringr)

# 1) 读入章节数据 ---------------------------------------
chap <- read_csv("around_world_80_days_clean_v2.csv", show_col_types = FALSE)

# 2) 计算每章情感（保持和 Figure 5 & 6 一样的做法） ------
sent_df <- chap %>%
  mutate(
    # 章节长度（字符数）
    chap_length = str_length(text),
    # 句子级平均情感
    raw_sent = map_dbl(text, ~ {
      sents  <- syuzhet::get_sentences(.x)
      scores <- syuzhet::get_sentiment(sents, "syuzhet")
      mean(scores, na.rm = TRUE)
    })
  ) %>%
  select(chapter, chap_length, raw_sent)

# 3) 把情感归一化到 0 ~ 0.4 区间（与前面图保持一致） -------
min_s <- min(sent_df$raw_sent, na.rm = TRUE)
max_s <- max(sent_df$raw_sent, na.rm = TRUE)

sent_df <- sent_df %>%
  mutate(
    sentiment = (raw_sent - min_s) / (max_s - min_s) * 0.4
  )

# 4) 绘制散点图 + 线性拟合线 -----------------------------
fig7 <- ggplot(sent_df, aes(x = chap_length, y = sentiment)) +
  geom_point(size = 2, colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "blue", linewidth = 0.7) +
  labs(
    title = "Correlation Between Chapter Length and Emotional Intensity",
    x     = "Chapter Length (characters)",
    y     = "Emotional Intensity (sentiment)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 0.4))   # 和前面图的情感范围保持一致

# 5) 保存图片 --------------------------------------------
ggsave("figure7_length_vs_sentiment.png",
       fig7, width = 7, height = 4, dpi = 300)

