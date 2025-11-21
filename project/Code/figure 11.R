pacman::p_load(tidyverse)

# 1) 手动输入各方法的召回率（这里用和你图差不多的数值，可按需要微调）
recall_df <- tribble(
  ~Method,      ~Recall,
  "Dictionary", 0.95,
  "Flair",      0.35,
  "spaCy",      0.55,
  "Stanza",     0.68
)

# 2) 绘图
fig11 <- ggplot(recall_df, aes(x = Method, y = Recall, fill = Method)) +
  geom_col(width = 0.7) +
  scale_fill_manual(
    values = c(
      "Dictionary" = "#F8766D",  # 和你图里的橙红色差不多
      "Flair"      = "#7CAE00",  # 绿色
      "spaCy"      = "#00BFC4",  # 青蓝色
      "Stanza"     = "#C77CFF"   # 紫色
    )
  ) +
  labs(
    title = "Recall of Location Extraction Methods",
    x     = "Method",
    y     = "Recall (Proportion of True Locations Detected)",
    fill  = "Method"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 1))   # 0 到 1 的比例

# 3) 保存图片
ggsave("figure11_location_recall.png",
       fig11, width = 6.5, height = 4, dpi = 300)
