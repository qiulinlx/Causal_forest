#load the package#
library(haven)
library(psych)
library(dplyr)

climatedata <- read.csv("data_ccb.csv")


data_clean <- climatedata %>%
  select(Belief.in.CC_1, Belief.in.CC_2, Belief.in.CC_4, Belief.in.CC_5, 
         WEPT8confirm, WEPT7confirm, WEPT6confirm, WEPT5confirm, WEPT4confirm, WEPT3confirm, WEPT2confirm, WEPT1confirm,
         CC_policy_1,	CC_policy_2,	CC_policy_3,	CC_policy_5	,CC_policy_6,	CC_policy_7,	CC_policy_8,	CC_policy_9	,CC_policy_10,
         MacArthur_SES, Trust_sci1_1, Trust_sci2_1, Share, country,, Trust_gov_1, ID_hum_1,
         Enviro_ID_1, Enviro_ID_2, Enviro_ID_3, Enviro_ID_4, 
         Gender, Age, Education.2, Politics2_1, Politics2_9, Income, 
         condName)

write.csv(data_clean, "clean.csv", row.names = FALSE)



df <- read.csv("clean.csv")

df <- df %>%
  rowwise() %>%
  mutate(
    policy_avg = sum(c_across(c(
      CC_policy_1, CC_policy_2, CC_policy_3, CC_policy_5,
      CC_policy_6, CC_policy_7, CC_policy_8, CC_policy_9, CC_policy_10
    )), na.rm = TRUE) / 9,
    
    belief_avg = sum(c_across(c(
      Belief.in.CC_1, Belief.in.CC_2,
      Belief.in.CC_4, Belief.in.CC_5
    )), na.rm = TRUE) / 4,
    
    WEPT_max = sum(c_across(c(
      WEPT8confirm, WEPT7confirm, WEPT6confirm, WEPT5confirm,
      WEPT4confirm, WEPT3confirm, WEPT2confirm, WEPT1confirm
    )), na.rm = TRUE)
  ) %>%
  ungroup()

df <- df %>%
  select(condName, belief_avg, WEPT_max, policy_avg , MacArthur_SES, 
         Share, country,
         Gender, Age, Education.2, Politics2_1, Politics2_9, Income)

# Save result
write.csv(df, "clean2.csv", row.names = FALSE)

