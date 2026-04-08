library(tidyverse)

# load up data
raw_df <- read_csv("G:\\Shared drives\\ADNI\\ADNI 2026\\UCBERKELEY_AMY_6MM_16Mar2026.csv")
demo <- read_csv("G:\\Shared drives\\ADNI\\ADNI 2026\\PTDEMOG_16Mar2026.csv", na = c("NA","-4"))
dx <- read_csv("G:\\Shared drives\\ADNI\\ADNI 2026\\DXSUM_16Mar2026.csv")

str(raw_df)
raw_df <- raw_df %>% select(RID, VISCODE, VISCODE2, CENTILOIDS, qc_flag)
dim(raw_df)

str(demo)
demo <- demo %>% filter(PHASE != "ADNI4") %>% select(RID, PTGENDER, PTDOB, HAS_QC_ERROR)
dim(demo)
demo <- demo[!duplicated(demo),]
demo <- demo %>% filter(!if_all(everything(), is.na))

str(dx)
dx <- dx %>% filter(PHASE != "ADNI4") %>% select(RID, VISCODE, VISCODE2, EXAMDATE, DIAGNOSIS, HAS_QC_ERROR)
dim(dx)

df <- left_join(raw_df, dx, by = c("RID","VISCODE","VISCODE2")) 
df <- df %>% rename(HAS_QC_ERROR.dx = HAS_QC_ERROR)
df <- left_join(df, demo, by = c("RID"))
df <- df %>% rename(HAS_QC_ERROR.demo = HAS_QC_ERROR,
                    qc_flag.pet = qc_flag)
dim(df)
df %>% View()

# inspect a little
df %>% group_by(RID) %>% summarize(n = n()) %>% arrange(desc(n)) %>% filter(n>1)
df %>% filter(RID == 74)

# remove other ADNI 4 indicators
df <- df %>% filter(!VISCODE %in% c("4_sc","4_bl","4_init","4_m12","4_m24","4_m36"))

compute_age <- function(dob_chr, exam_date) {
  dob <- as.Date(paste0("01/", dob_chr), format = "%d/%m/%Y")
  age <- as.numeric(difftime(exam_date, dob, units = "days")) / 365.25
  return(age)
}
df %>% str()
df <- df %>% mutate(AGE = compute_age(PTDOB, EXAMDATE))

df

write_csv(df, "G:\\Shared drives\\ADNI\\ADNI 2026\\Data files\\CL_Dx_demog.csv")

## QC stuff
demo %>% count(HAS_QC_ERROR) %>% mutate(pct = n / sum(n) * 100)
dx %>% count(HAS_QC_ERROR) %>% mutate(pct = n / sum(n) * 100)
raw_df %>% count(qc_flag) %>% mutate(pct = n / sum(n) * 100)

demo %>% filter(!is.na(HAS_QC_ERROR)) %>% count(HAS_QC_ERROR) %>% mutate(pct = n / sum(n) * 100)
dx %>% filter(!is.na(HAS_QC_ERROR)) %>% count(HAS_QC_ERROR) %>% mutate(pct = n / sum(n) * 100)
raw_df %>% filter(!is.na(qc_flag)) %>% count(qc_flag) %>% mutate(pct = n / sum(n) * 100)


## for raw_df via UC BERKELEY
# Quality control flag based on visual inspection: 
# 2 = Pass; 
# 1 = Partial pass; 
# 0 = Fail;
# -1 = Not assessed;
# -2 = Cannot be processed; See UCBERKELEY Amyloid Processing Methods PDF on LONI for details.

## for demo and dx
# 0 = Does not have QC error or QC error has been approved; 1 = Has QC error




