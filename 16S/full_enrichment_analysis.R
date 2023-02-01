library(ggthemes)
library(RColorBrewer)
library(glue)

# ---- Loading Data ----
source("C:/Users/Zjardyn/Desktop/PhD/16S/16S_functions.R")

all_tbl <- "C:/Users/Zjardyn/Desktop/full_out/full-merged-table.qza"
all_tx <- "C:/Users/Zjardyn/Desktop/full_out/full-merged-taxonomy.qza"
all_sums <- taxa_sums(table = all_tbl, taxonomy = all_tx)

all_tab <- taxa_prop_table(taxasums = all_sums, taxa = "Genus")
all_tab_f <- filter_prop_table(all_tab, threshold = 5)
all_tbm <- as_tibble(melt(as.matrix(all_tab_f)))

smp_filt = rownames(all_tab)[!rownames(all_tab) == 'C']
all_tbm_f <- filter_samples(smp_filt = smp_filt, table = all_tab_f, table_melt = all_tbm)

# ---- Soil ----
smp_filt_soil <- c("A1.1", "A2.3" , "A3.3" , "A4.2")
all_tbm_soil <- filter_samples(smp_filt = smp_filt_soil, table = all_tab_f, table_melt = all_tbm)

soil_A <- lubridate::ymd(20220607)
soil_B <-lubridate::ymd(20220620)

all_tbm_soil_r <- all_tbm_soil %>%
  mutate(Var1 =  str_replace_all(Var1, "^A1\\.[:digit:]", glue("S1_{soil_A}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^A2\\.[:digit:]", glue("S2_{soil_A}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^A3\\.[:digit:]", glue("S3_{soil_A}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^A4\\.[:digit:]", glue("S4_{soil_A}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^B1\\.[:digit:]", glue("S1_{soil_B}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^B2\\.[:digit:]", glue("S2_{soil_B}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^B3\\.[:digit:]", glue("S3_{soil_B}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^B4\\.[:digit:]", glue("S4_{soil_B}")))

num_col <- all_tbm_soil_r %>%
  summarise(orgs = unique(Var2)) %>%
  nrow()

new_cols <- lrg_colors(smp = num_col, seed = 1)
all_tbm_soil_r %>%
  ggplot(aes(fill=Var2, y=value, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = new_cols)

# ---- Enrichments ----
smp_filt_enrich <- rownames(all_tab)
smp_filt_enrich[-c(1:18, 27:30, 65:67)]


# ---- Standardize Samples ----

soil_A <- lubridate::ymd(20220607)
soil_B <-lubridate::ymd(20220620)

rownames(all_tab)

all_tbm_f %>%
  # Soil June07
  mutate(Var1 =  str_replace_all(Var1, "^A1\\.[:digit:]", glue("Soil_1_{lubridate::ymd(20220607)}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^A2\\.[:digit:]", glue("Soil_2_{lubridate::ymd(20220607)}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^A3\\.[:digit:]", glue("Soil_3_{lubridate::ymd(20220607)}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^A4\\.[:digit:]", glue("Soil_4_{lubridate::ymd(20220607)}"))) %>%
  # Soil June20
  mutate(Var1 =  str_replace_all(Var1, "^B1\\.[:digit:]", glue("Soil_1_{lubridate::ymd(20220620)}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^B2\\.[:digit:]", glue("Soil_2_{lubridate::ymd(20220620)}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^B3\\.[:digit:]", glue("Soil_3_{lubridate::ymd(20220620)}"))) %>%
  mutate(Var1 =  str_replace_all(Var1, "^B4\\.[:digit:]", glue("Soil_4_{lubridate::ymd(20220620)}"))) %>%
  # Hex Nov11
  mutate(Var1 = str_replace_all(Var1, "^H\\.1\\.Nov11_S[:digit:]*", glue("Hex_1_{lubridate::ymd(20221111)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^H\\.2\\.Nov11_S[:digit:]*", glue("Hex_2_{lubridate::ymd(20221111)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^H\\.3\\.Nov11_S[:digit:]*", glue("Hex_3_{lubridate::ymd(20221111)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^H\\.4\\.Nov11_S[:digit:]*", glue("Hex_4_{lubridate::ymd(20221111)}"))) %>%
  # Hex Nov18 
  mutate(Var1 = str_replace_all(Var1, "^H\\.1\\.Nov18_S[:digit:]*", glue("Hex_1_{lubridate::ymd(20221118)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^H\\.2\\.Nov18_S[:digit:]*", glue("Hex_2_{lubridate::ymd(20221118)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^H\\.3\\.Nov18_S[:digit:]*", glue("Hex_3_{lubridate::ymd(20221118)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^H\\.4\\.Nov18_S[:digit:]*", glue("Hex_4_{lubridate::ymd(20221118)}"))) %>%
  # HD Dec2
  mutate(Var1 = str_replace_all(Var1, "^HD\\.1\\.Dec2_S[:digit:]*", glue("HexDTE_1_{lubridate::ymd(20221202)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^HD\\.2\\.Dec2_S[:digit:]*", glue("HexDTE_2_{lubridate::ymd(20221202)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^HD\\.3\\.Dec2_S[:digit:]*", glue("HexDTE_3_{lubridate::ymd(20221202)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^HD\\.4\\.Dec2_S[:digit:]*", glue("HexDTE_4_{lubridate::ymd(20221202)}"))) %>%
  # Hex July20
  mutate(Var1 = str_replace_all(Var1, "^July20\\.Hexadecane\\.1", glue("Hex_1_{lubridate::ymd(20220720)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July20\\.Hexadecane\\.2", glue("Hex_2_{lubridate::ymd(20220720)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July20\\.Hexadecane\\.3", glue("Hex_3_{lubridate::ymd(20220720)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July20\\.Hexadecane\\.4", glue("Hex_4_{lubridate::ymd(20220720)}"))) %>%
  # Sty July20
  mutate(Var1 = str_replace_all(Var1, "^July20\\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220720)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July20\\.Styrene\\.2", glue("Sty_2_{lubridate::ymd(20220720)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July20\\.Styrene\\.3", glue("Sty_3_{lubridate::ymd(20220720)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July20\\.Styrene\\.4", glue("Sty_4_{lubridate::ymd(20220720)}"))) %>%
  # Hex July29
  mutate(Var1 = str_replace_all(Var1, "^July29\\.Hexadecane\\.1", glue("Hex_1_{lubridate::ymd(20220729)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July29\\.Hexadecane\\.2", glue("Hex_2_{lubridate::ymd(20220729)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July29\\.Hexadecane\\.3", glue("Hex_3_{lubridate::ymd(20220729)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July29\\.Hexadecane\\.4", glue("Hex_4_{lubridate::ymd(20220729)}"))) %>%
  # Sty July29
  mutate(Var1 = str_replace_all(Var1, "^July29\\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220729)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July29\\.Styrene\\.2", glue("Sty_2_{lubridate::ymd(20220729)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July29\\.Styrene\\.3", glue("Sty_3_{lubridate::ymd(20220729)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July29\\.Styrene\\.4", glue("Sty_4_{lubridate::ymd(20220729)}"))) %>%
  # Hex July7
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Hexadecane\\.1", glue("Hex_1_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Hexadecane\\.2", glue("Hex_2_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Hexadecane\\.3", glue("Hex_3_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Hexadecane\\.4", glue("Hex_4_{lubridate::ymd(20220707)}"))) %>%
  # Sty July7
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Styrene\\.2", glue("Sty_2_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Styrene\\.3", glue("Sty_3_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Styrene\\.4", glue("Sty_4_{lubridate::ymd(20220707)}"))) %>%
  # Sty June21
  mutate(Var1 = str_replace_all(Var1, "^July7 \\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Styrene\\.2", glue("Sty_2_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Styrene\\.3", glue("Sty_3_{lubridate::ymd(20220707)}"))) %>%
  mutate(Var1 = str_replace_all(Var1, "^July7\\.Styrene\\.4", glue("Sty_4_{lubridate::ymd(20220707)}"))) %>%
  
  print(n = 100)
