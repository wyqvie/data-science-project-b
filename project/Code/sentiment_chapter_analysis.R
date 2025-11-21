# ===========================
# Figure 1: Chapter-level sentiment trend
# ===========================

pacman::p_load(tidyverse, tidytext, syuzhet)

# ---- 1) 读取清洗后的小说文本 ----
book_df <- read_csv("around_world_80_days_clean_v2.csv")

# ---- 2) 计算章节平均情感分数 ----
# 如果你想用 NRC/Bing/AFINN 等词典平均值，可用 syuzhet::get_sentiment
book_df <- book_df %>%
  mutate(
    sentiment = get_sentiment(text, method = "syuzhet")  # 可换 "bing" 或 "afinn"
  )

# ---- 3) 绘制章节情感趋势图 ----
figure1 <- book_df %>%
  ggplot(aes(x = chapter, y = sentiment)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "darkblue") +
  labs(
    title = "Figure 1. Sentiment Trend Across Chapters",
    x = "Chapter",
    y = "Average Sentiment Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# ---- 4) 保存图像 ----
ggsave("figure1_sentiment_trend.png", figure1, width = 8, height = 5, dpi = 300)

