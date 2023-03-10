library(ggthemes)
library(RColorBrewer)
library(glue)
library(tidyr)
# load my functions
# source("C:/Users/Zjardyn/Desktop/PhD/16S/16S_functions.R")
source("C:/Users/zjard/Desktop/PhD/16S/16S_functions.R")

# ---- Loading Data ----
all_tbl <- "C:/Users/zjard/Desktop/full_out/full-merged-table.qza"
all_tx <- "C:/Users/zjard/Desktop/full_out/full-merged-taxonomy.qza"
all_sums <- taxa_sums(table = all_tbl, taxonomy = all_tx)
all_tab <- taxa_prop_table(taxasums = all_sums, taxa = "Genus")

# ---- Standardize sample naming ----
all_tab <- all_tab %>%
  rownames_to_column(var = "smp") %>%
  # Soil June07
  mutate(smp =  str_replace_all(smp, "^A1\\.1", glue("Soil_1_1_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A1\\.2", glue("Soil_1_2_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A1\\.3", glue("Soil_1_3_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A1\\.4", glue("Soil_1_4_{lubridate::ymd(20220607)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^A2\\.1", glue("Soil_2_1_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A2\\.2", glue("Soil_2_2_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A2\\.3", glue("Soil_2_3_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A2\\.4", glue("Soil_2_4_{lubridate::ymd(20220607)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^A3\\.1", glue("Soil_3_1_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A3\\.2", glue("Soil_3_2_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A3\\.3", glue("Soil_3_3_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A3\\.4", glue("Soil_3_4_{lubridate::ymd(20220607)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^A4\\.1", glue("Soil_4_1{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A4\\.2", glue("Soil_4_2{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A4\\.3", glue("Soil_4_3{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A4\\.4", glue("Soil_4_4{lubridate::ymd(20220607)}"))) %>%
  # Soil June20
  mutate(smp =  str_replace_all(smp, "^B1\\.1", glue("Soil_1_1_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B1\\.2", glue("Soil_1_2_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B1\\.3", glue("Soil_1_3_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B1\\.4", glue("Soil_1_4_{lubridate::ymd(20220620)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^B2\\.1", glue("Soil_2_1_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B2\\.2", glue("Soil_2_2_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B2\\.3", glue("Soil_2_3_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B2\\.4", glue("Soil_2_4_{lubridate::ymd(20220620)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^B3\\.1", glue("Soil_3_1_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B3\\.2", glue("Soil_3_2_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B3\\.3", glue("Soil_3_3_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B3\\.4", glue("Soil_3_4_{lubridate::ymd(20220620)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^B4\\.1", glue("Soil_4_1{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B4\\.2", glue("Soil_4_2{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B4\\.3", glue("Soil_4_3{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B4\\.4", glue("Soil_4_4{lubridate::ymd(20220620)}"))) %>%
  # Hex Nov11
  mutate(smp = str_replace_all(smp, "^H\\.1\\.Nov11_S[:digit:]*", glue("Hex_1_{lubridate::ymd(20221111)}"))) %>%
  mutate(smp = str_replace_all(smp, "^H\\.2\\.Nov11_S[:digit:]*", glue("Hex_2_{lubridate::ymd(20221111)}"))) %>%
  mutate(smp = str_replace_all(smp, "^H\\.3\\.Nov11_S[:digit:]*", glue("Hex_3_{lubridate::ymd(20221111)}"))) %>%
  mutate(smp = str_replace_all(smp, "^H\\.4\\.Nov11_S[:digit:]*", glue("Hex_4_{lubridate::ymd(20221111)}"))) %>%
  # Hex Nov18 
  mutate(smp = str_replace_all(smp, "^H\\.1\\.Nov18_S[:digit:]*", glue("Hex_1_{lubridate::ymd(20221118)}"))) %>%
  mutate(smp = str_replace_all(smp, "^H\\.2\\.Nov18_S[:digit:]*", glue("Hex_2_{lubridate::ymd(20221118)}"))) %>%
  mutate(smp = str_replace_all(smp, "^H\\.3\\.Nov18_S[:digit:]*", glue("Hex_3_{lubridate::ymd(20221118)}"))) %>%
  mutate(smp = str_replace_all(smp, "^H\\.4\\.Nov18_S[:digit:]*", glue("Hex_4_{lubridate::ymd(20221118)}"))) %>%
  # HD Dec2
  mutate(smp = str_replace_all(smp, "^HD\\.1\\.Dec2_S[:digit:]*", glue("HexDTE_1_{lubridate::ymd(20221202)}"))) %>%
  mutate(smp = str_replace_all(smp, "^HD\\.2\\.Dec2_S[:digit:]*", glue("HexDTE_2_{lubridate::ymd(20221202)}"))) %>%
  mutate(smp = str_replace_all(smp, "^HD\\.3\\.Dec2_S[:digit:]*", glue("HexDTE_3_{lubridate::ymd(20221202)}"))) %>%
  mutate(smp = str_replace_all(smp, "^HD\\.4\\.Dec2_S[:digit:]*", glue("HexDTE_4_{lubridate::ymd(20221202)}"))) %>%
  # Hex July20
  mutate(smp = str_replace_all(smp, "^July20\\.Hexadecane\\.1", glue("Hex_1_{lubridate::ymd(20220720)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July20\\.Hexadecane\\.2", glue("Hex_2_{lubridate::ymd(20220720)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July20\\.Hexadecane\\.3", glue("Hex_3_{lubridate::ymd(20220720)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July20\\.Hexadecane\\.4", glue("Hex_4_{lubridate::ymd(20220720)}"))) %>%
  # Sty July20
  mutate(smp = str_replace_all(smp, "^July20\\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220720)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July20\\.Styrene\\.2", glue("Sty_2_{lubridate::ymd(20220720)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July20\\.Styrene\\.3", glue("Sty_3_{lubridate::ymd(20220720)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July20\\.Styrene\\.4", glue("Sty_4_{lubridate::ymd(20220720)}"))) %>%
  # Hex July29
  mutate(smp = str_replace_all(smp, "^July29\\.Hexadecane\\.1", glue("Hex_1_{lubridate::ymd(20220729)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July29\\.Hexadecane\\.2", glue("Hex_2_{lubridate::ymd(20220729)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July29\\.Hexadecane\\.3", glue("Hex_3_{lubridate::ymd(20220729)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July29\\.Hexadecane\\.4", glue("Hex_4_{lubridate::ymd(20220729)}"))) %>%
  # Sty July29
  mutate(smp = str_replace_all(smp, "^July29\\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220729)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July29\\.Styrene\\.2", glue("Sty_2_{lubridate::ymd(20220729)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July29\\.Styrene\\.3", glue("Sty_3_{lubridate::ymd(20220729)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July29\\.Styrene\\.4", glue("Sty_4_{lubridate::ymd(20220729)}"))) %>%
  # Hex July7
  mutate(smp = str_replace_all(smp, "^July7\\.Hexadecane\\.1", glue("Hex_1_{lubridate::ymd(20220707)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July7\\.Hexadecane\\.2", glue("Hex_2_{lubridate::ymd(20220707)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July7\\.Hexadecane\\.3", glue("Hex_3_{lubridate::ymd(20220707)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July7\\.Hexadecane\\.4", glue("Hex_4_{lubridate::ymd(20220707)}"))) %>%
  # Sty July7
  mutate(smp = str_replace_all(smp, "^July7\\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220707)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July7\\.Styrene\\.2", glue("Sty_2_{lubridate::ymd(20220707)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July7\\.Styrene\\.3", glue("Sty_3_{lubridate::ymd(20220707)}"))) %>%
  mutate(smp = str_replace_all(smp, "^July7\\.Styrene\\.4", glue("Sty_4_{lubridate::ymd(20220707)}"))) %>%
  # Sty June21
  mutate(smp = str_replace_all(smp, "^June21 \\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220621)}"))) %>%
  mutate(smp = str_replace_all(smp, "^June21\\.Styrene\\.2", glue("Sty_2_{lubridate::ymd(20220621)}"))) %>%
  mutate(smp = str_replace_all(smp, "^June21\\.Styrene\\.3", glue("Sty_3_{lubridate::ymd(20220621)}"))) %>%
  mutate(smp = str_replace_all(smp, "^June21\\.Styrene\\.4", glue("Sty_4_{lubridate::ymd(20220621)}"))) %>%
  # Sty Nov11
  mutate(smp = str_replace_all(smp, "^S\\.1\\.Nov11_S[:digit:]*", glue("Sty_1_{lubridate::ymd(20221111)}"))) %>%
  mutate(smp = str_replace_all(smp, "^S\\.2\\.Nov11_S[:digit:]*", glue("Sty_2_{lubridate::ymd(20221111)}"))) %>%
  mutate(smp = str_replace_all(smp, "^S\\.3\\.Nov11_S[:digit:]*", glue("Sty_3_{lubridate::ymd(20221111)}"))) %>%
  mutate(smp = str_replace_all(smp, "^S\\.4\\.Nov11_S[:digit:]*", glue("Sty_4_{lubridate::ymd(20221111)}"))) %>%
  # Sty Nov18 
  mutate(smp = str_replace_all(smp, "^S\\.1\\.Nov18_S[:digit:]*", glue("Sty_1_{lubridate::ymd(20221118)}"))) %>%
  mutate(smp = str_replace_all(smp, "^S\\.2\\.Nov18_S[:digit:]*", glue("Sty_2_{lubridate::ymd(20221118)}"))) %>%
  mutate(smp = str_replace_all(smp, "^S\\.3\\.Nov18_S[:digit:]*", glue("Sty_3_{lubridate::ymd(20221118)}"))) %>%
  mutate(smp = str_replace_all(smp, "^S\\.4\\.Nov18_S[:digit:]*", glue("Sty_4_{lubridate::ymd(20221118)}"))) %>%
  # SD Dec2
  mutate(smp = str_replace_all(smp, "^SD\\.1\\.Nov11_S[:digit:]*", glue("StyDTE_1_{lubridate::ymd(20221202)}"))) %>%
  mutate(smp = str_replace_all(smp, "^SD\\.2\\.Nov11_S[:digit:]*", glue("StyDTE_2_{lubridate::ymd(20221202)}"))) %>%
  mutate(smp = str_replace_all(smp, "^SD\\.3\\.Nov11_S[:digit:]*", glue("StyDTE_3_{lubridate::ymd(20221202)}"))) %>%
  mutate(smp = str_replace_all(smp, "^SD\\.4\\.Nov11_S[:digit:]*", glue("StyDTE_4_{lubridate::ymd(20221202)}"))) %>%
  column_to_rownames(var = "smp")

# ---- Soils ----
soils_filt <- rownames(all_tab)[str_detect(rownames(all_tab), "Soil")]
soils_tab <- filter_samples_table(soils_filt, all_tab)
soils_tab_thresh <- thresh_prop_table(soils_tab, threshold = 5)
soils_tbm <- as_tibble(melt(as.matrix(soils_tab_thresh)))

soils_tbm <- arrange_taxa(soils_tab_thresh, soils_tbm)

# split_func <- function(x, by) {
#   r <- diff(range(x))
#   out <- seq(0, r - by - 1, by = by)
#   c(round(min(x) + c(0, out - 0.51 + (max(x) - max(out)) / 2), 0), max(x))
# }
# idx <- split_func(1:length(all_cols_blind_hex), 10)
# new_cols <- all_cols_blind_hex[idx]

library(viridis)
all_cols_blind <- viridis::viridis.map %>%
  filter(opt == "A")
all_cols_blind <- all_cols_blind[30:216,]
# all_cols_blind1 <- all_cols_blind %>%
#   filter(opt == "A")
# all_cols_blind1 <- all_cols_blind1[40:236,]
# 
# all_cols_blind2 <- all_cols_blind %>%
#   filter(opt == "G")
# all_cols_blind2 <- all_cols_blind2[40:236,]
# 
# all_cols_blind <- rbind(all_cols_blind1, all_cols_blind2)
all_cols_blind_hex <- rgb(all_cols_blind[,1],all_cols_blind[,2],all_cols_blind[,3])

all_cols_blind_hex <- all_cols_blind_hex[c(TRUE, FALSE)]

num_col <- soils_tbm %>%
  summarise(orgs = unique(Var2)) %>%
  nrow()

set.seed(5)
new_cols <- sample(all_cols_blind_hex, num_col)
new_cols[1] <- "#808080"

soils_tbm %>%
  ggplot(aes(fill=Var2, y=value, x=Var1)) +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = new_cols)

 # # remove controls
# smp_filt = rownames(all_tab)[!rownames(all_tab) == 'C']
# all_tab_f <- filter_samples_table(smp_filt = smp_filt, table = all_tab)
# View(all_tab_f)


# ---- Enrichments ----
enrich_filt <- rownames(all_tab)[str_detect(rownames(all_tab), "^Sty_|^Hex_" )]
enrich_tab <- filter_samples_table(enrich_filt, all_tab)
enrich_tab_thresh <- thresh_prop_table(enrich_tab, threshold = 10)
enrich_tbm <- as_tibble(melt(as.matrix(enrich_tab_thresh)))
enrich_tbm <- arrange_taxa(enrich_tab_thresh, enrich_tbm)

# Add variables for plotting
enrich_tbm_v <- enrich_tbm %>% 
  separate_wider_delim(Var1, delim = "_", names = c("Treatment", "Soil", "Date"))

### Hexadecane ###

enrich_hex <- enrich_tbm_v %>%
    filter(Treatment == "Hex") %>%
    filter(value != 0.0)

enrich_hex <- droplevels(enrich_hex)

enrich_tab_hex <- enrich_tab_thresh %>%
    rownames_to_column(var = "smp") %>%
    filter(str_detect(smp, "Hex")) %>%
    column_to_rownames(var = "smp") 

enrich_hex <- arrange_taxa(enrich_tab_hex, enrich_hex)

num_col_hex <- enrich_hex %>%
    summarise(orgs = unique(Var2)) %>%
    nrow()

set.seed(4)

new_cols_hex <- sample(all_cols_blind_hex, num_col2)
new_cols_hex[1] <- "#808080"
    
enrich_hex %>%
    ggplot(aes(fill=Var2, y=value, x=Date)) + 
    geom_bar(position="fill", stat="identity") + 
    theme(axis.text.x= element_text(angle = 90, hjust = 1)) + 
    scale_fill_manual(values = new_cols_hex) +
    facet_grid(Treatment~Soil) 
### Styrene ###

enrich_sty <- enrich_tbm_v %>%
    filter(Treatment == "Sty") %>%
    filter(value != 0.0) 

enrich_sty <- droplevels(enrich_sty)

enrich_tab_sty <- enrich_tab_thresh %>%
    rownames_to_column(var = "smp") %>%
    filter(str_detect(smp, "Sty")) %>%
    column_to_rownames(var = "smp")

enrich_sty <- arrange_taxa(enrich_tab_sty, enrich_sty)

num_col_sty <- enrich_sty %>%
    summarise(orgs = unique(Var2)) %>%
    nrow()

set.seed(3)

new_cols_sty <- sample(all_cols_blind_hex, num_col2)
new_cols_sty[1] <- "#808080"

enrich_sty %>%
  ggplot(aes(fill=Var2, y=value, x=Date)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = new_cols_sty) +
  facet_grid(Treatment~Soil) 

# ---- DTE ---- 
dte_filt <- rownames(all_tab)[str_detect(rownames(all_tab), "DTE")]
dte_tab <- filter_samples_table(dte_filt, all_tab)
dte_tab_thresh <- thresh_prop_table(dte_tab, threshold = 0.0001)
dte_tbm <- as_tibble(melt(as.matrix(dte_tab_thresh)))
dte_tbm <- arrange_taxa(dte_tab_thresh, dte_tbm)

dte_tbm %>%
  ggplot(aes(fill=Var2, y=value, x=Var1)) +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = new_cols)

