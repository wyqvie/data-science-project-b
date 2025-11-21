pacman::p_load(tidyverse, tidytext, sentimentr, readr)

# 读取清洗后的文本文件（你的 38 个章节的 clean_v2）
df <- read_csv("around_world_80_days_clean_v2.csv", show_col_types = FALSE)

# token 化（用于词典匹配）
word_tokens <- df %>%
  unnest_tokens(word, text)

# 加载 lexicon
afinn <- get_sentiments("afinn")
bing  <- get_sentiments("bing")
nrc   <- get_sentiments("nrc")

# AFINN
afinn_scores <- word_tokens %>%
  inner_join(afinn, by = "word") %>%
  group_by(chapter) %>%
  summarise(afinn_sent = mean(value, na.rm = TRUE))

# Bing
bing_scores <- word_tokens %>%
  inner_join(bing, by = "word") %>%
  mutate(score = ifelse(sentiment == "positive", 1, -1)) %>%
  group_by(chapter) %>%
  summarise(bing_sent = mean(score, na.rm = TRUE))

# NRC (正向 - 负向)
nrc_scores <- word_tokens %>%
  inner_join(nrc, by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  mutate(score = ifelse(sentiment == "positive", 1, -1)) %>%
  group_by(chapter) %>%
  summarise(nrc_sent = mean(score, na.rm = TRUE))

# SentimentR
sentiment_sent <- sentiment(df$text)
sentiment_sent <- sentiment_sent %>%
  group_by(element_id) %>%
  summarise(sentimatrix_sent = mean(sentiment))

# 合并 4 套结果
sent_res <- df %>%
  select(chapter) %>%
  left_join(afinn_scores, by = "chapter") %>%
  left_join(bing_scores, by = "chapter") %>%
  left_join(nrc_scores, by = "chapter") %>%
  left_join(sentiment_sent, by = c("chapter" = "element_id"))

# 导出为图 3 使用的文件
write_csv(sent_res, "sentiment_lexicon_results.csv")

# 1. 读入刚才生成的情感结果文件
sent_res <- read_csv("sentiment_lexicon_results.csv", show_col_types = FALSE)

# 2. 选取四个情感列并转成长格式
sent_long <- sent_res %>%
  select(afinn_sent, bing_sent, nrc_sent, sentimatrix_sent) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "method",
    values_to = "score"
  )

# 3. 画箱线图（就是你图里那张）
p_box <- ggplot(sent_long, aes(x = method, y = score)) +
  geom_boxplot(fill = "white", colour = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of Chapter-Level Sentiment Scores\nAcross Lexicon Methods",
    x = "",
    y = "Sentiment Score"
  )

# 显示
print(p_box)

# 4. 可选：保存为图片，用在报告和 GitHub
ggsave("fig_lexicon_boxplot.png",
       plot = p_box, width = 10, height = 6, dpi = 300)

