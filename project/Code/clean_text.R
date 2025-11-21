## Load Libraries ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, tidytext, gutenbergr, stringr, readr)

## 1. Load Book from Project Gutenberg ----

book <- gutenberg_download(103, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")

## 2. Cleaning: Remove Header (and optionally Footer) ----
# 原始代码只去掉前 53 行 header，这里保持一致
book_text_only <- book %>%
  tail(nrow(book) - 53)

## 3. Cleaning: Separate Book by Chapters ----

# 根据行是否匹配 "chapter i / chapter 1 / chapter v ..." 来累计章节号
book_chapters <- book_text_only %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(
      text,
      regex("^chapter [\\divxlc]", ignore_case = TRUE)
    ))
  ) %>%
  ungroup()

# 去掉空行
book_chapters_clean <- book_chapters %>%
  filter(text != "")

# 去掉章节标题行（保留正文）
ch <- 0              # 当前章节号
rows <- c()          # 需要删除的行号

for (i in 1:nrow(book_chapters_clean)) {
  
  temp_ch <- book_chapters_clean[i, "chapter", drop = TRUE]
  
  # 如果进入了新章节（chapter 发生变化）
  if (!(temp_ch == ch)) {
    ch <- temp_ch
    rows <- c(rows, i)
  } else if (temp_ch == ch) {
    # 若这一行仍在同一章节，且整行是全大写/数字/标点/空白，则视为章节标题或非正文
    if (str_detect(
      book_chapters_clean[i, "text", drop = TRUE],
      "^([[:upper:]]|[[:digit:]]|[[:punct:]]|[[:blank:]])+$"
    )) {
      rows <- c(rows, i)
    }
  }
}

rows <- unique(rows)

book_chapters_clean2 <- book_chapters_clean %>%
  filter(!row_number() %in% rows)

# 合并每一章的所有行，变成一行长文本
book_chapters_clean3 <- book_chapters_clean2 %>%
  select(chapter, text) %>%
  group_by(chapter) %>%
  mutate(text = paste0(text, collapse = " ")) %>%
  slice(1) %>%
  ungroup()


df0 <- book_chapters_clean3 %>%
  mutate(
    chapter = as.integer(chapter),
    text    = ifelse(is.na(text), "", text),
    text    = str_squish(text)
  )

# 为安全起见，按当前顺序重置章节编号为 1..N
df0 <- df0 %>%
  arrange(chapter) %>%
  mutate(chapter = row_number())

# 定义：删除脚注/插图/编者注等非叙事内容
clean_non_narrative <- function(x) {
  x %>%
    str_replace_all("\\[[^\\]]*\\]", " ") %>%      # [Illustration], [Note], etc.
    str_replace_all("\\*[^*]*\\*", " ") %>%        # *Footnote* style
    str_replace_all("\\(\\s*see[^)]*\\)", " ") %>% # (See Note) etc.
    str_squish()
}

# 定义：标点和符号规范化（保留 . ? ! ' - 及空格和单词）
normalize_punct <- function(x) {
  x %>%
    str_replace_all("[“”]", "\"") %>%
    str_replace_all("[‘’]", "'") %>%
    str_replace_all("—", "-") %>%
    str_replace_all("[^\\w\\s\\.'\\?\\!-]", " ") %>%  # 其他符号替换为空格
    str_replace_all("-{2,}", "-") %>%                 # 多个 - 压成一个
    str_replace_all("\\.{3,}", "..") %>%              # 多个 . 压成两个
    str_squish()
}

df1 <- df0 %>%
  mutate(
    text = clean_non_narrative(text),
    text = normalize_punct(text),
    text = str_squish(text)
  )

# 可选：统一转为小写，便于情感词典/地名匹配
df1 <- df1 %>%
  mutate(text = str_to_lower(text))

# 简单 QC：字数、词数
df_qc <- df1 %>%
  mutate(
    n_chars  = nchar(text),
    n_tokens = str_count(text, "\\b\\w+\\b")
  ) %>%
  select(chapter, n_chars, n_tokens)

# 导出最终清洗结果
write_csv(df1,  "around_world_80_days_clean_v2.csv")
write_csv(df_qc, "cleaning_qc_summary_v2.csv")

message("✅ Cleaning completed:\n",
        " - around_world_80_days_clean_v2.csv\n",
        " - cleaning_qc_summary_v2.csv\n",
        " - Chapters: ", nrow(df1))
