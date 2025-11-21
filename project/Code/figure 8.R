pacman::p_load(tidyverse, readr, syuzhet)

# 1) 读取段落级 BERT 结果 -----------------------------
bert_df <- read_csv("bert_para.csv", show_col_types = FALSE)

# ---- 统一列名 ----------------------------------------
if (!"chapter" %in% names(bert_df)) {
  cand <- names(bert_df)[grepl("^chap", tolower(names(bert_df)))]
  if (length(cand) >= 1) {
    bert_df <- bert_df %>% rename(chapter = !!cand[1])
  }
}

if (!"text" %in% names(bert_df)) {
  cand_txt <- names(bert_df)[grepl("text|para", tolower(names(bert_df)))]
  if (length(cand_txt) >= 1) {
    bert_df <- bert_df %>% rename(text = !!cand_txt[1])
  } else {
    stop("找不到段落文本列，请检查 bert_para.csv。")
  }
}

get_bert_col <- function(df) {
  nm <- names(df)
  if ("bert_score" %in% nm) return(df$bert_score)
  for (k in c("sentiment", "score", "probability", "logit")) {
    if (k %in% nm && is.numeric(df[[k]])) return(df[[k]])
  }
  if (all(c("prob_pos","prob_neg") %in% nm)) {
    return(df$prob_pos - df$prob_neg)
  }
  stop("找不到 BERT 数值列，请检查 bert_para.csv。")
}

bert_df <- bert_df %>%
  mutate(
    chapter = as.integer(chapter),
    bert_score = get_bert_col(cur_data())
  )

# 2) 只保留要展示的章节（例：5,10,15,20,22）-------------
chapters_keep <- c(5, 10, 15, 20, 22)

bert_sub <- bert_df %>%
  filter(chapter %in% chapters_keep)

# 3) 词典法平均 ----------------------------------------
lex_df <- bert_sub %>%
  mutate(
    nrc   = get_sentiment(text, method = "syuzhet"),
    bing  = get_sentiment(text, method = "bing"),
    afinn = get_sentiment(text, method = "afinn"),
    lexicon_mean = (nrc + bing + afinn) / 3
  ) %>%
  select(chapter, lexicon_mean, bert_score)

# 4) 绘图：散点 + y = x 线 + y轴固定范围 ----------------
fig_bert_vs_lex <- ggplot(
  lex_df,
  aes(x = lexicon_mean, y = bert_score, colour = factor(chapter))
) +
  geom_point(alpha = 0.8, size = 1.8) +
  geom_abline(slope = 1, intercept = 0, colour = "black") +
  labs(
    title   = "Paragraph-level sentiment: BERT vs lexicon mean",
    x       = "Lexicon mean sentiment (NRC + Bing + AFINN)",
    y       = "BERT sentiment",
    colour  = "Chapter"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(-2, 2))    # ⭐⭐ 关键修改：固定 y 轴范围

# 5) 保存图片 -------------------------------------------
ggsave("figure8_bert_vs_lexicon_mean.png",
       fig_bert_vs_lex, width = 7, height = 4, dpi = 300)

