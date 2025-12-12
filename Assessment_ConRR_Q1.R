library(dplyr)
library(stringr)

library(readxl)
library(dplyr)
library(purrr)


df_A <- read_excel("A.xlsx")
df_B <- read_excel("B.xlsx")
df_P <- read_excel("P.xlsx")
df_SE <- read_excel("SouthEast.xlsx")
df_B1 <- read_excel("Book1.xlsx")

library(dplyr)

# 1. df_A, df_B, df_P 这里的列名已经是标准的了，暂时不需要改动
# 但为了保险，我们可以确认一下 (或者跳过)

# 2. 修改 df_SE
# 注意：你的数据里是 "Phusical" (有个拼写错误)，这里需要对应上
df_SE <- df_SE %>%
  rename(
    PH = Phusical,   # 将 Phusical 重命名为 PH
    MH = Mental      # 将 Mental 重命名为 MH
    # ID 列保留或删除看你需求
  ) %>%
  # 调整顺序以匹配 df_A
  select(Age, Region, PH, MH, Smoker, Belief, SES5, Gender, everything())

# 3. 修改 df_B1
# 这里包含空格和特殊符号，需要加引号
df_B1 <- df_B1 %>%
  rename(
    PH = `Physical Health`,
    MH = `Mental Health`,
    ID = `No.`
  )


names(df_A)
names(df_B)
names(df_P)
names(df_SE)
names(df_B1)



library(dplyr)

# 建议步骤：在合并前，给每个表加一列标记来源 (可选，但强烈建议)
# 这样合并后你还能知道哪行数据来自哪个原始表
df_A$Source <- "A"
df_B$Source <- "B"
df_P$Source <- "P"
df_SE$Source <- "SE"
df_B1$Source <- "B1"

# 核心步骤：合并所有 dataframe
# bind_rows 会自动对齐列名，df_B1 缺少的 Region/SES5 会自动变成 NA
df_total <- bind_rows(df_A, df_B, df_P, df_SE, df_B1)

library(dplyr)

# 1. 修正 df_B1 的 Age 列类型
# as.numeric 会把纯数字的字符串转成数字
# 如果原本有像 "20s", "unknown" 这种非数字内容，会变成 NA (这是正常的)
df_B1$Age <- as.numeric(df_B1$Age)
df_B1$ID <- as.character(df_B1$ID)



df_model <- df_total %>%
  mutate(
    # standardise strings
    Belief = str_to_lower(str_trim(as.character(Belief))),
    PH     = str_to_lower(str_trim(as.character(PH))),
    MH     = str_to_lower(str_trim(as.character(MH))),
    # map common encodings to 0/1, everything else -> NA
    Belief = case_when(
      Belief %in% c("1","y","yes","true","t") ~ 1,
      Belief %in% c("0","n","no","false","f") ~ 0,
      TRUE ~ NA_real_
    ),
    PH = case_when(
      PH %in% c("1","y","yes","true","t") ~ 1,
      PH %in% c("0","n","no","false","f") ~ 0,
      TRUE ~ NA_real_
    ),
    MH = case_when(
      MH %in% c("1","y","yes","true","t") ~ 1,
      MH %in% c("0","n","no","false","f") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(Belief), !is.na(PH), !is.na(MH))
# sanity checks that prevent wasted time
stopifnot(all(df_model$Belief %in% c(0,1)))
stopifnot(length(unique(df_model$Belief)) == 2)
stopifnot(length(unique(df_model$PH)) == 2)
stopifnot(length(unique(df_model$MH)) == 2)

# logistic regression with interaction
model <- glm(
  Belief ~ PH * MH,
  data   = df_model,
  family = binomial(link = "logit")
)

summary(model)
