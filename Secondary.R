# Secondary 

df_base2 <- df_model %>% filter(!is.na(Belief), !is.na(PH), !is.na(MH))

m_adj_noSES <- glm(
  Belief ~ PH * MH + Age + Gender + Smoker + Region + Source,
  data = df_base2,
  family = binomial(),
  na.action = na.omit
)

summary(m_adj_noSES)
nobs(m_adj_noSES)
