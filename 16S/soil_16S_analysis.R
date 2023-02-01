library(ggthemes)
library(RColorBrewer)
library(glue)

source("C:/Users/Zjardyn/Desktop/PhD/16S/16S_functions.R")

all_tbl <- "C:/Users/Zjardyn/Desktop/full_out/full-merged-table.qza"
all_tx <- "C:/Users/Zjardyn/Desktop/full_out/full-merged-taxonomy.qza"
all_sums <- taxa_sums(table = all_tbl, taxonomy = all_tx)

all_tab <- taxa_prop_table(taxasums = all_sums, taxa = "Genus")
all_tab_f <- filter_prop_table(all_tab, threshold = 2)
all_tbm <- as_tibble(melt(as.matrix(all_tab_f)))

smp_filt = rownames(all_tab)[!rownames(all_tab) == 'C']
# smp_filt <- c("A1.1", "A2.3" , "A3.3" , "A4.2")


all_tbm_f <- filter_samples(smp_filt = smp_filt, table = all_tab_f, table_melt = all_tbm)

new_cols <- lrg_colors(smp = ncol(all_tab_f), seed = 1)

all_tbm_f %>%
  ggplot(aes(fill=Var2, y=value, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = new_cols)

soil_sums <- taxa_sums(table = soil_tbl, taxonomy = soil_tx)
soil_tab <- taxa_prop_table(taxasums = soil_sums, taxa = "Genus")
soil_tab_f <- filter_prop_table(soil_tab, threshold = 2)
soil_tbm <- as_tibble(melt(as.matrix(soil_tab_f)))

smp_filt <- c("A1.1", "A2.3" , "A3.3" , "A4.2")

soil_tbm_f <- filter_samples(smp_filt = smp_filt, table = soil_tab, table_melt = soil_tbm)


cols <- lrg_colors(smp = ncol(soil_tab_f), seed = 1)

soil_tbm_f %>%
  ggplot(aes(fill=Var2, y=value, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = new_cols)

soil_tbm_f %>%
    mutate(Var1 = str_replace_all(Var1, "A1.1", "One")) %>%
    mutate(Var1 = str_replace_all(Var1, "A2.3", "Two")) %>%
    mutate(Var1 = str_replace_all(Var1, "A3.3", "Three")) %>%
    mutate(Var1 = str_replace_all(Var1, "A4.2", "Four")) %>%
    mutate(Var1 = fct_relevel(Var1, "One", "Two", "Three", "Four")) %>%
    
    ggplot( aes(fill=Var2, y=value, x=Var1)) + 
    geom_bar(position="fill", stat="identity") +
    scale_y_continuous(labels=scales::percent) +
    xlab("Soil") + ylab("Abundance (%)") + 
    # labs(fill=info[1]) +
    # theme_solarized_2() + 
    # theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
    # ggtitle(glue('16S Soil, Threshold: {info[2]}%, Number of taxa in Other: {info[3]}')) +
    # scale_fill_brewer(palette = "Paired") +
    scale_fill_manual(values = cols) +
    # scale_fill_viridis_d() +
    theme(axis.text.x = element_text(size = 14)) +
    # theme_q2r() + 
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    theme(legend.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 12))

enrichment_tbl <- "C:/Users/Zjardyn/Desktop/enrichment_out/enrichment-table.qza"
enrichment_tx <- "C:/Users/Zjardyn/Desktop/enrichment_out/enrichment-taxonomy.qza"

new_tbl <- "C:/Users/Zjardyn/Desktop/new_out/table.qza"
new_tx <- "C:/Users/Zjardyn/Desktop/new_out/taxonomy.qza"

new_tab <- taxa_prop_table(table = new_tbl, taxonomy = new_tx, taxa = "Genus")
new_tab_f <- filter_prop_table(new_tab, threshold = 2)
new_tbm <- as_tibble(melt(as.matrix(new_tab_f)))
new_tbm_f <- filter_samples(smp_filt = rownames(new_tab), table = new_tab, table_melt = new_tbm)

new_cols <- lrg_colors(smp = ncol(new_tab_f), seed = 2)

new_tbm_f %>%
  ggplot(aes(fill=Var2, y=value, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = new_cols)
