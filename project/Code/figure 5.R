pacman::p_load(tidyverse, readr, syuzhet, purrr)

# 1) 读入章节数据 ---------------------------------------
chap <- read_csv("around_world_80_days_clean_v2.csv", show_col_types = FALSE)

# 2) 计算每章情感（先分句，再算平均） -------------------
sent_df <- chap %>%
  mutate(
    raw_sent = map_dbl(text, ~ {
      sents  <- syuzhet::get_sentences(.x)            # 分句
      scores <- syuzhet::get_sentiment(sents, "syuzhet")
      mean(scores, na.rm = TRUE)                      # 每章句子平均
    })
  ) %>%
  select(chapter, raw_sent)

# 3) 把 raw_sent 归一化到 0 ~ 0.4 区间（和旧图差不多） ----
min_s <- min(sent_df$raw_sent, na.rm = TRUE)
max_s <- max(sent_df$raw_sent, na.rm = TRUE)

sent_df <- sent_df %>%
  mutate(
    sentiment = (raw_sent - min_s) / (max_s - min_s) * 0.4
  )

# 4) 定义三个叙事阶段 -----------------------------------
stage_df <- tribble(
  ~stage,               ~xmin, ~xmax,
  "Europe–Middle East",     1,    12,
  "Asia–Pacific",          13,    24,
  "America–London",        25,    37
)

y_label <- max(sent_df$sentiment, na.rm = TRUE) * 1.05

# 5) 绘图 ------------------------------------------------
fig5 <- ggplot(sent_df, aes(x = chapter, y = sentiment)) +
  # 背景阶段块
  geom_rect(
    data = stage_df,
    aes(xmin = xmin - 0.5, xmax = xmax + 0.5,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey90", alpha = 0.5
  ) +
  # 情感轨迹
  geom_line(size = 0.6, colour = "black") +
  geom_point(size = 1.5, colour = "black") +
  # 阶段标签
  geom_text(
    data = stage_df,
    aes(x = (xmin + xmax) / 2, y = y_label, label = stage),
    inherit.aes = FALSE,
    size = 3
  ) +
  labs(
    title = "Sentiment Trajectory with Narrative Stages",
    x = "Chapter",
    y = "Sentiment Score"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# 6) 保存图片 --------------------------------------------
ggsave("figure5_sentiment_trajectory_stages.png",
       fig5, width = 7, height = 4, dpi = 300)

