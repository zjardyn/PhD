library(ggthemes)
library(RColorBrewer)
library(glue)
library(stringr.tools)
# load my functions
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
  mutate(smp =  str_replace_all(smp, "^A1\\.1", glue("Soil.1_1_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A1\\.2", glue("Soil.1_2_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A1\\.3", glue("Soil.1_3_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A1\\.4", glue("Soil.1_4_{lubridate::ymd(20220607)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^A2\\.1", glue("Soil.2_1_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A2\\.2", glue("Soil.2_2_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A2\\.3", glue("Soil.2_3_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A2\\.4", glue("Soil.2_4_{lubridate::ymd(20220607)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^A3\\.1", glue("Soil.3_1_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A3\\.2", glue("Soil.3_2_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A3\\.3", glue("Soil.3_3_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A3\\.4", glue("Soil.3_4_{lubridate::ymd(20220607)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^A4\\.1", glue("Soil.4_1_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A4\\.2", glue("Soil.4_2_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A4\\.3", glue("Soil.4_3_{lubridate::ymd(20220607)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^A4\\.4", glue("Soil.4_4_{lubridate::ymd(20220607)}"))) %>%
  # Soil June20
  mutate(smp =  str_replace_all(smp, "^B1\\.1", glue("Soil.1_1_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B1\\.2", glue("Soil.1_2_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B1\\.3", glue("Soil.1_3_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B1\\.4", glue("Soil.1_4_{lubridate::ymd(20220620)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^B2\\.1", glue("Soil.2_1_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B2\\.2", glue("Soil.2_2_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B2\\.3", glue("Soil.2_3_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B2\\.4", glue("Soil.2_4_{lubridate::ymd(20220620)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^B3\\.1", glue("Soil.3_1_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B3\\.2", glue("Soil.3_2_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B3\\.3", glue("Soil.3_3_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B3\\.4", glue("Soil.3_4_{lubridate::ymd(20220620)}"))) %>%
  
  mutate(smp =  str_replace_all(smp, "^B4\\.1", glue("Soil.4_1_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B4\\.2", glue("Soil.4_2_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B4\\.3", glue("Soil.4_3_{lubridate::ymd(20220620)}"))) %>%
  mutate(smp =  str_replace_all(smp, "^B4\\.4", glue("Soil.4_4_{lubridate::ymd(20220620)}"))) %>%
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
  mutate(smp = str_replace_all(smp, "^June21\\.Styrene\\.1", glue("Sty_1_{lubridate::ymd(20220621)}"))) %>%
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
soil_thresh <- 3
soils_filt <- rownames(all_tab)[str_detect(rownames(all_tab), "Soil")]
soils_tab <- filter_samples_table(soils_filt, all_tab)
soils_tab_thresh <- thresh_prop_table(soils_tab, threshold = soil_thresh)
soils_tbm <- as_tibble(melt(as.matrix(soils_tab_thresh)))

soils_tbm <- soils_tbm %>%
  filter(Var1 == "Soil.1_1_2022-06-07" | 
         Var1 == "Soil.2_3_2022-06-07" | 
         Var1 == "Soil.3_3_2022-06-20" | 
         Var1 == "Soil.4_2_2022-06-20" )

soils_tbm <- arrange_taxa(soils_tab_thresh, soils_tbm)

soils_tbm_v <- soils_tbm %>% 
    separate_wider_delim(Var1, delim = "_", names = c("Sample","Replicate", "Date"), cols_remove = FALSE)

# soils_cols <- viridis_colours(soils_tbm_v, seed = 1, start_trunc = 30, end_trunc = 0)

soils_cols <-lrg_colors(length(unique(soils_tbm_v$Var2)), seed = 43)

size = -5

soils_tbm_v %>%
  mutate(Sample = str_replace_all(Sample, "\\.", " ")) %>%
  ggplot(aes(fill=Var2, y=value, x=Sample)) +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = soils_cols) +
  guides(fill=guide_legend(title="Genus")) +
  labs(y = "Abundance", title = glue("16S community composition of soil samples. Threshold: {soil_thresh}%")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text = element_text(size = 13 + size),
        axis.title = element_text(size = 13 + size),
        legend.title = element_text(size = 13+ size ),
        legend.text = element_text(size = 12 + size),
        plot.title = element_text(size = 17 + size),
        legend.key.size = unit(.38, 'cm')) 
    
#export 720p
ggsave("fig1.png", path = "C:/Users/zjard/Pictures", width = 1920, height = 1080, units = "px" )
# ---- Enrichment ----
enrich_thresh = 6
enrich_filt <- rownames(all_tab)[str_detect(rownames(all_tab), "^Sty_|^Hex_" )]
enrich_tab <- filter_samples_table(enrich_filt, all_tab)
enrich_tab_thresh <- thresh_prop_table(enrich_tab, threshold = enrich_thresh)
enrich_tbm <- as_tibble(melt(as.matrix(enrich_tab_thresh)))
enrich_tbm <- arrange_taxa(enrich_tab_thresh, enrich_tbm)

# Add variables for plotting
enrich_tbm_v <- enrich_tbm %>% 
  separate_wider_delim(Var1, delim = "_", names = c("Treatment", "Soil", "Date"))

enrich_tbm_v_d <- enrich_tbm_v %>%
    mutate(Datename = paste(lubridate::day(Date), lubridate::month(Date, label = T),lubridate::year(Date) , sep = "-")) %>%
    mutate(Datename = as.factor(Datename))  %>%
    mutate(Datename = fct_relevel(Datename, c("21-Jun-2022","7-Jul-2022", "20-Jul-2022", "29-Jul-2022", "11-Nov-2022", "18-Nov-2022")))

### Hexadecane ###
enrich_hex <- enrich_tbm_v_d %>%
    filter(Treatment == "Hex") %>%
    filter(value != 0.0)

enrich_hex <- droplevels(enrich_hex)

enrich_tab_hex <- enrich_tab_thresh %>%
    rownames_to_column(var = "smp") %>%
    filter(str_detect(smp, "Hex")) %>%
    column_to_rownames(var = "smp") 

enrich_hex <- arrange_taxa(enrich_tab_hex, enrich_hex)

enrich_hex <- enrich_hex %>%
  mutate(Soil = str_prefix(Soil, "Soil "))

hex_colours <-lrg_colors(length(unique(enrich_hex$Var2)), seed = 30)

size2 <- -5

enrich_hex %>%
  ggplot(aes(fill=Var2, y=value, x=Datename)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = hex_colours) +
  facet_wrap(~Soil, nrow = 1) +
  guides(fill=guide_legend(title="Genus")) +
  labs(y = "Abundance", x = "Date",
       title = glue("16S community succession of hexadecane enrichments. Threshold: {enrich_thresh}%")) + 
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12 + size2, angle = 35, vjust = 1.1, hjust=1),
        axis.text.y = element_text(size = 12 + size2),
        axis.title = element_text(size = 13 + size2),
        legend.title = element_text(size = 13 + size2),
        legend.text = element_text(size = 12 + size2),
        strip.text.x = element_text(size = 13+ size2),
        legend.key.size = unit(.38, 'cm'),
        plot.title = element_text(size = 12))
    
ggsave("fig2.png", path = "C:/Users/zjard/Pictures", width = 1920, height = 1080, units = "px" )

### Styrene ###
enrich_sty <- enrich_tbm_v_d %>%
    filter(Treatment == "Sty") %>%
    filter(value != 0.0) 

enrich_sty <- droplevels(enrich_sty)

enrich_tab_sty <- enrich_tab_thresh %>%
    rownames_to_column(var = "smp") %>%
    filter(str_detect(smp, "Sty")) %>%
    column_to_rownames(var = "smp")

enrich_sty <- arrange_taxa(enrich_tab_sty, enrich_sty)

enrich_sty <- enrich_sty %>%
  mutate(Soil = str_prefix(Soil, "Soil "))

sty_colours <-lrg_colors(length(unique(enrich_sty$Var2)), seed = 43)

enrich_sty %>%
  ggplot(aes(fill=Var2, y=value, x=Datename)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = sty_colours) +
  facet_wrap(~Soil, nrow = 1) +
  guides(fill=guide_legend(title="Genus")) +
  labs(y = "Abundance", x = "Date",
       title = glue("16S community composition of styrene enrichments. Threshold: {enrich_thresh}%")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12 + size2, angle = 35, vjust = 1.1, hjust=1),
         axis.text.y = element_text(size = 12 + size2),
         axis.title = element_text(size = 13 + size2),
         legend.title = element_text(size = 13 + size2),
         legend.text = element_text(size = 12 + size2),
         strip.text.x = element_text(size = 13+ size2),
         legend.key.size = unit(.38, 'cm'),
         plot.title = element_text(size = 12),
        legend.justification = c(1,0.7))

ggsave("fig3.png", path = "C:/Users/zjard/Pictures", width = 1920, height = 1080, units = "px" )

# ---- DTE ---- 
dte_filt <- rownames(all_tab)[str_detect(rownames(all_tab), "DTE")]
dte_tab <- filter_samples_table(dte_filt, all_tab)
dte_tab_thresh <- thresh_prop_table(dte_tab, threshold = 0.0001)
dte_tbm <- as_tibble(melt(as.matrix(dte_tab_thresh)))
dte_tbm <- arrange_taxa(dte_tab_thresh, dte_tbm)

dte_colours <- viridis_colours(dte_tbm, seed = 1, start_trunc = 30, end_trunc = 20)

dte_tbm %>%
  ggplot(aes(fill=Var2, y=value, x=Var1)) +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = dte_colours)


# ---- Shannon Diversity ----
metadata<-read_q2metadata("C:/Users/zjard/Desktop/enrichment/sample-metadata.tsv")
shannon<-read_qza("C:/Users/zjard/Desktop/enrichment/shannon_vector.qza")
shannon<-shannon$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged
# gplots::venn(list(metadata=metadata$SampleID, shannon=shannon$SampleID))

shannon_m<-
    metadata %>% 
    left_join(shannon)


# metadata$date <- factor(metadata$date, levels = c("June21", "July7", "July20", "July29"))


shannon_v <- shannon_m %>%
    filter(!is.na(shannon_entropy)) %>%
    mutate(soil = str_to_sentence(soil)) %>%
    mutate(soil = fct_relevel(soil, "One", "Two", "Three", "Four")) %>%
    mutate(date = gsub("([a-zA-Z])([0-9])", "\\1-\\2", date, perl=TRUE)) %>%
    mutate(date = paste(date, "2022", sep = "-")) %>%
    mutate(date = str_replace_all(date, "July", "Jul")) %>%
    mutate(date = str_replace_all(date, "June", "Jun")) %>%
    mutate(date = gsub("(.*)-(.*)-(.*)", "\\2-\\1-\\3", date)) %>%
    mutate(date = as.factor(date))  %>%
    mutate(date = fct_relevel(date, c("21-Jun-2022","7-Jul-2022", "20-Jul-2022", "29-Jul-2022")))

size3 <- -5
shannon_v %>%
    ggplot(aes(x=date, y=shannon_entropy, color=soil, group = interaction(treatment, soil)), alpha = 0.8) +
    geom_point(size = 3, alpha = .8) +
    geom_line(linewidth = 1, alpha = .8) +
    theme_bw() + 
    labs(y = "Shannon Diversity", x = "Date", title = "Alpha diversity estimate of monomer enrichments using the Shannon index.") + 
    scale_color_viridis_d(name="Soil") + 
    facet_wrap(~treatment) +
    theme_bw() + 
    theme(axis.text.x = element_text(size = 12 + size3, angle = 35, vjust = 1.1, hjust=1), 
            axis.text.y = element_text(size = 12 + size3),
           axis.title = element_text(size = 14 + size3),
          plot.title = element_text(size = 17 + size3), 
          legend.text = element_text(size = 13 + size3), 
          legend.title = element_text(size = 15+ size3),
          strip.text.x = element_text(size = 14 + size3))

ggsave("fig4.png", path = "C:/Users/zjard/Pictures", width = 1920, height = 1080, units = "px" )

 # ---- PCoA ----    
uwunifrac<-read_qza("C:/Users/zjard/Desktop/enrichment/unweighted_unifrac_pcoa_results.qza")
shannon<-read_qza("C:/Users/zjard/Desktop/enrichment/shannon_vector.qza")$data %>% rownames_to_column("SampleID") 

PCoA <- uwunifrac$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    left_join(shannon) %>%
    mutate(soil = str_to_sentence(soil)) %>%
    mutate(soil = fct_relevel(soil, "One", "Two", "Three", "Four")) %>%
    mutate(date = gsub("([a-zA-Z])([0-9])", "\\1-\\2", date, perl=TRUE)) %>%
    mutate(date = paste(date, "2022", sep = "-")) %>%
    mutate(date = str_replace_all(date, "July", "Jul")) %>%
    mutate(date = str_replace_all(date, "June", "Jun")) %>%
    mutate(date = gsub("(.*)-(.*)-(.*)", "\\2-\\1-\\3", date)) %>%
    mutate(date = as.factor(date))  %>%
    mutate(date = fct_relevel(date, c("21-Jun-2022","7-Jul-2022", "20-Jul-2022", "29-Jul-2022")))

size4 <- -5

PCoA %>%
    ggplot(aes(x=PC1, y=PC2, color=date, alpha=treatment, shape = soil)) +
    geom_point(size = 4) + 
    # scale_shape_manual(values=c(16,1), name="Monomer") + 
    scale_color_discrete(name="Date") +
    scale_alpha_discrete(name = "Monomer", range = c(0.3, 0.95))+ 
    scale_shape_discrete(name = "Soil") +
    labs(title = "PCoA of monomer enrichments.") + 
    theme_bw() + 
    theme(axis.text = element_text(size = 12 + size4),
          axis.title = element_text(size = 13 + size4),
          plot.title = element_text(size = 17 + size4),
          legend.text = element_text(size = 12 + size4),
          legend.title = element_text(size = 13 + size4),
          legend.key.size = unit(.38, 'cm'))

ggsave("fig5.png", path = "C:/Users/zjard/Pictures", width = 1920, height = 1080, units = "px" )
