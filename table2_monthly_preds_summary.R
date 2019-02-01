library(tidyverse)

# load data--------
perna_pel <- read_csv('data/perna_pel_predictions.csv')
perna_wai <- read_csv('data/perna_wai_predictions.csv')

# table categorical catches-----------
table(perna_wai$cat_perna)/nrow(perna_wai)

table_wai <- 
  table(perna_wai$cat_perna,perna_wai$month) %>% 
  data.frame() %>% 
  rename(Cat = Var1, Month = Var2) %>% 
  group_by(Month) %>% 
  mutate(n = sum(Freq)) %>% 
  group_by(Month, Cat) %>% 
  mutate(perc = Freq/n*100) %>% 
  select(-Freq, - n) %>% 
  spread(Month,perc)%>% 
  mutate(Site = 'Wainui')


table_pel <- 
  table(perna_pel$cat_perna,perna_pel$month) %>% 
  data.frame() %>% 
  rename(Cat = Var1, Month = Var2) %>% 
  group_by(Month) %>% 
  mutate(n = sum(Freq)) %>% 
  group_by(Month, Cat) %>% 
  mutate(perc = Freq/n*100) %>% 
  select(-Freq, - n) %>% 
  spread(Month,perc) %>% 
  mutate(Site = 'Clova')

bind_rows(table_wai, table_pel) %>% 
  select(Site, everything()) %>% 
  write_csv('tables/monthly_preds.csv')
