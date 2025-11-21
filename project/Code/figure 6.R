# ===========================
# Figure 6: Sentiment Distribution Across Narrative Stages
# ===========================

pacman::p_load(tidyverse, readr, syuzhet, purrr)

# 1) 读入章节数据 ---------------------------------------
chap <- read_csv("around_world_80_days_clean_v2.csv", show_col_types = FALSE)

# 2) 计算每章情感（保持和 Figure 5 一样的做法） ----------
sent_df <- chap %>%
  mutate(
    raw_sent = map_dbl(text, ~ {
      sents  <- syuzhet::get_sentences(.x)            # 分句
      scores <- syuzhet::get_sentiment(sents, "syuzhet")
      mean(scores, na.rm = TRUE)                      # 每章句子平均
    })
  ) %>%
  select(chapter, raw_sent)

# 3) 归一化到 0 ~ 0.4 区间（与 Figure 5 一致） -----------
min_s <- min(sent_df$raw_sent, na.rm = TRUE)
max_s <- max(sent_df$raw_sent, na.rm = TRUE)

sent_df <- sent_df %>%
  mutate(
    sentiment = (raw_sent - min_s) / (max_s - min_s) * 0.4
  )

# 4) 按章节分配叙事阶段 ----------------------------------
sent_stage <- sent_df %>%
  mutate(
    stage = case_when(
      chapter >= 1  & chapter <= 12 ~ "Europe–Middle East",
      chapter >= 13 & chapter <= 24 ~ "Asia–Pacific",
      chapter >= 25 & chapter <= 37 ~ "America–London",
      TRUE ~ NA_character_
    ),
    # 让 x 轴顺序和截图一致
    stage = factor(stage,
                   levels = c("America–London",
                              "Asia–Pacific",
                              "Europe–Middle East"))
  ) %>%
  filter(!is.na(stage))

# 5) 绘制箱线图 -----------------------------------------
fig6 <- ggplot(sent_stage, aes(x = stage, y = sentiment, fill = stage)) +
  geom_boxplot(alpha = 0.6, colour = "black") +
  scale_fill_manual(
    values = c("America–London"   = "#F8766D",  # 红
               "Asia–Pacific"     = "#00BA38",  # 绿
               "Europe–Middle East" = "#619CFF")  # 蓝
  ) +
  labs(
    title  = "Sentiment Distribution Across Narrative Stages",
    x      = "Narrative Stage",
    y      = "Sentiment Score",
    fill   = "Stage"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 0.4))   # 让纵轴范围与 Figure 5 对齐

# 6) 保存图片 -------------------------------------------
ggsave("figure6_stage_boxplot.png",
       fig6, width = 7, height = 4, dpi = 300)

message("✅ Figure 6 已生成：figure6_stage_boxplot.png")
