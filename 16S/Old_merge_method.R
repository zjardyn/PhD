library(ggthemes)
library(RColorBrewer)
library(glue)

source("C:/Users/Zjardyn/Desktop/PhD/16S/16S_functions.R")

soil_tbl <- "C:/Users/Zjardyn/Desktop/soil_out/soil-table.qza"
soil_tx <- "C:/Users/Zjardyn/Desktop/soil_out/taxonomy.qza"

enrichment_tbl <- "C:/Users/Zjardyn/Desktop/enrichment_out/enrichment-table.qza"
enrichment_tx <- "C:/Users/Zjardyn/Desktop/enrichment_out/enrichment-taxonomy.qza"

new_tbl <- "C:/Users/Zjardyn/Desktop/new_out/table.qza"
new_tx <- "C:/Users/Zjardyn/Desktop/new_out/taxonomy.qza"


soil_sums <- taxa_sums(table = soil_tbl, taxonomy = soil_tx)
enrichment_sums <- taxa_sums(table = enrichment_tbl, taxonomy = enrichment_tx)
new_sums <- taxa_sums(table = new_tbl, taxonomy = new_tx)

soil_sums_v <- soil_sums %>%
  rownames_to_column(var = "taxa")

enrichment_sums_v <- enrichment_sums %>%
  rownames_to_column(var = "taxa")

new_sums_v <- new_sums %>%
  rownames_to_column(var = "taxa")

most_sums <- full_join(soil_sums_v, enrichment_sums_v, by = "taxa") %>%
  mutate_all(~replace(., is.na(.), 0))

all_sums <-full_join(most_sums, new_sums_v, by = "taxa") %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  column_to_rownames(var = "taxa")