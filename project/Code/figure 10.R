
pacman::p_load(tidyverse)

# 1) 构造错误统计数据 --------------------------
ner_errors <- tribble(
  ~error_type,                 ~Flair, ~spaCy, ~Stanza,
  "Mislabeled as Org",           4,      2,      0,
  "Mislabeled as Person",        3,      2,      0,
  "Missed Location",             6,      5,      5,
  "Spelling Errors",             0,      1,      0
)

# 2) 转成长格式（ggplot 绘图需要） ---------------
ner_long <- ner_errors %>%
  pivot_longer(cols = c(Flair, spaCy, Stanza),
               names_to = "Model",
               values_to = "Count")

# 3) 绘图 ---------------------------------------
fig10 <- ggplot(ner_long, aes(x = error_type, y = Count, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(
    "Flair"  = "#F8766D",
    "spaCy"  = "#00BA38",
    "Stanza" = "#619CFF"
  )) +
  labs(
    title = "Comparison of NER Errors by Model",
    x = "Type of Error",
    y = "Number of Errors",
    fill = "Model"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

# 4) 保存图片 -------------------------------------
ggsave("figure10_ner_error_comparison.png",
       fig10, width = 6.5, height = 4, dpi = 300)
