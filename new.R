library(readxl)
library(dplyr)
library(stringr)
library(purrr)

df_A  <- read_excel("A.xlsx")         %>% mutate(Source = "A")
df_B  <- read_excel("B.xlsx")         %>% mutate(Source = "B")
df_P  <- read_excel("P.xlsx")         %>% mutate(Source = "P")

df_SE <- read_excel("SouthEast.xlsx") %>%
  rename(PH = Phusical, MH = Mental) %>%
  mutate(Source = "SE") %>%
  select(any_of(c("Age","Region","PH","MH","Smoker","Belief","SES5","Gender","ID","Source")), everything())

df_B1 <- read_excel("Book1.xlsx") %>%
  rename(PH = `Physical Health`, MH = `Mental Health`, ID = `No.`) %>%
  mutate(
    Age    = suppressWarnings(as.numeric(Age)),
    ID     = as.character(ID),
    Source = "B1"
  ) %>%
  select(any_of(c("ID","Age","Region","PH","MH","Smoker","Belief","SES5","Gender","Source")), everything())

df_total <- bind_rows(df_A, df_B, df_P, df_SE, df_B1)

to01 <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  case_when(
    x %in% c("1","y","yes","true","t")  ~ 1,
    x %in% c("0","n","no","false","f")  ~ 0,
    TRUE                               ~ NA_real_
  )
}

df_model <- df_total %>%
  mutate(
    Belief = to01(Belief),
    PH     = to01(PH),
    MH     = to01(MH)
  ) %>%
  filter(!is.na(Belief), !is.na(PH), !is.na(MH))

print(table(df_model$Belief, useNA = "ifany"))
print(table(df_model$PH, useNA = "ifany"))
print(table(df_model$MH, useNA = "ifany"))

if (nrow(df_model) == 0) stop("No complete cases after cleaning: check encodings in Belief/PH/MH.")
if (length(unique(df_model$Belief)) < 2) stop("Belief has <2 classes after cleaning.")
if (length(unique(df_model$PH)) < 2) stop("PH has <2 classes after cleaning.")
if (length(unique(df_model$MH)) < 2) stop("MH has <2 classes after cleaning.")

model <- glm(Belief ~ PH * MH, data = df_model, family = binomial(link = "logit"))
summary(model)
