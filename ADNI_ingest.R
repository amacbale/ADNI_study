library(tidyverse)

# load up data
raw_df <- read_csv("G:\\Shared drives\\ADNI\\ADNI 2026\\Data files\\UCBERKELEY_AMY_6MM_16Mar2026.csv")
demo <- read_csv("G:\\Shared drives\\ADNI\\ADNI 2026\\Data files\\PTDEMOG_16Mar2026.csv", na = c("NA","-4"))
dx <- read_csv("G:\\Shared drives\\ADNI\\ADNI 2026\\Data files\\DXSUM_16Mar2026.csv")

str(raw_df)
raw_df <- raw_df %>% select(RID, VISCODE, VISCODE2, CENTILOIDS)
dim(raw_df)

str(demo)
demo <- demo %>% filter(PHASE != "ADNI4") %>% select(RID, PTGENDER, PTDOBYY)
dim(demo)
demo <- demo[!duplicated(demo),]
demo <- demo %>% drop_na()

str(dx)
dx <- dx %>% filter(PHASE != "ADNI4") %>% select(RID, VISCODE, VISCODE2, EXAMDATE, DIAGNOSIS)
dim(dx)

df <- left_join(raw_df, dx, by = c("RID","VISCODE","VISCODE2"))      
df <- left_join(df, demo, by = c("RID"))
dim(df)
df %>% View()

# inspect a little
df %>% group_by(RID) %>% summarize(n = n()) %>% arrange(desc(n)) %>% filter(n>1)
df %>% filter(RID == 74)

# remove other ADNI 4 indicators
df <- df %>% filter(!VISCODE %in% c("4_sc","4_bl","4_init","4_m12","4_m24","4_m36"))

compute_age <- function(dob, exam_date) {
  age <- as.numeric(difftime(exam_date, dob, units = "days")) / 365.25
  return(age)
}
df <- df %>% mutate(AGE = compute_age(PTDOBYY, EXAMDATE))


