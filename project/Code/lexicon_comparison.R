# 03_sentiment_lexicon_comparison.R
# 从章节级 clean 文本出发，计算 NRC / Bing / AFINN / Combined
# 的章节情感得分，并画出多方法情感轨迹图。

pacman::p_load(tidyverse, tidytext, syuzhet)

#--------------------------------------------------
# 1. 读入章节级 clean 文本
#--------------------------------------------------

chapters <- readr::read_csv("around_world_80_days_clean_v2.csv",
                            show_col_types = FALSE)

#--------------------------------------------------
# 2. 拆成词，准备连接情感词典
#--------------------------------------------------

tokens <- chapters %>%
  unnest_tokens(word, text) %>%     # 每行一个词
  filter(!str_detect(word, "^[0-9]+$"))   # 去掉纯数字（可选）

#--------------------------------------------------
# 3. 载入三个常用情感词典：NRC / Bing / AFINN
#--------------------------------------------------

nrc <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  mutate(score_nrc = if_else(sentiment == "positive", 1, -1)) %>%
  select(word, score_nrc)

bing <- get_sentiments("bing") %>%
  mutate(score_bing = if_else(sentiment == "positive", 1, -1)) %>%
  select(word, score_bing)

afinn <- get_sentiments("afinn") %>%
  rename(score_afinn = value)       # 正值=正面，负值=负面

#--------------------------------------------------
# 4. 连接到 token，得到每个 token 的多词典得分
#--------------------------------------------------

tokens_sent <- tokens %>%
  left_join(nrc,  by = "word") %>%
  left_join(bing, by = "word") %>%
  left_join(afinn, by = "word")

#--------------------------------------------------
# 5. 汇总到章节：每章的平均情感得分
#   （你也可以改成 sum / 中位数，这里用 mean，方便比较）
#--------------------------------------------------

sentiment_chapter <- tokens_sent %>%
  group_by(chapter) %>%
  summarise(
    nrc_sent   = mean(score_nrc,   na.rm = TRUE),
    bing_sent  = mean(score_bing,  na.rm = TRUE),
    afinn_sent = mean(score_afinn, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 为了跟你图里 5 条线一致，构造一个简单的“组合得分”
  mutate(
    combined_sent = rowMeans(across(c(nrc_sent, bing_sent, afinn_sent)),
                             na.rm = TRUE),
    syuzhet_sent  = get_sentiment(chapters$text, method = "syuzhet")
    %>% as.numeric() %>% scale() %>% as.numeric()
  )

#--------------------------------------------------
# 6. 为画图做长表：method 一列，score 一列
#--------------------------------------------------

sentiment_long <- sentiment_chapter %>%
  select(chapter, nrc_sent, bing_sent, afinn_sent,
         combined_sent, syuzhet_sent) %>%
  pivot_longer(
    cols = -chapter,
    names_to = "Method",
    values_to = "Score"
  ) %>%
  mutate(
    Method = factor(Method,
                    levels = c("afinn_sent",
                               "bing_sent",
                               "nrc_sent",
                               "combined_sent",
                               "syuzhet_sent"),
                    labels = c("AFINN",
                               "Bing",
                               "NRC",
                               "Combined (mean of three)",
                               "Syuzhet (chapter)"))
  )

#--------------------------------------------------
# 7. 画出多词典情感轨迹图
#--------------------------------------------------

p <- ggplot(sentiment_long,
            aes(x = chapter, y = Score,
                colour = Method, group = Method)) +
  geom_line(size = 0.7) +
  geom_point(size = 1.5) +
  labs(
    title = "Chapter-level Sentiment Trajectories Across Lexicon Methods",
    x = "Chapter",
    y = "Normalised Sentiment Score",
    colour = "Method"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# 显示图
print(p)

# 保存成图片）
ggsave("fig_lexicon_sentiment_trajectories.png",
       plot = p, width = 10, height = 6, dpi = 300)
