library(qiime2R)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(glue)

#---- User Settings ----#
taxa = "Genus"
threshold = 2

metadata<-read_q2metadata("meta-data.txt")
SVs <-read_qza("soil-table.qza")$data
taxonomy <-read_qza("taxonomy.qza")$data %>% parse_taxonomy()
taxasums <-summarize_taxa(SVs, taxonomy)$Species

# Their way
# taxa_barplot(taxasums, metadata, "soil")

# My way

# # rownames are concatenated taxa, pull out then make it a column
# taxasums <- as_tibble(rownames_to_column(taxasums, var = "taxon"))
# 
# # separate that into a column for each taxa
# taxasums <- separate(taxasums, taxon, c("Kingdom", "Phylum", 
#                                         "Class", "Order", 
#                                         "Family", "Genus", 
#                                         "Species"), sep = ";")
# 
# # if its a string remove whitespace
# taxasums <- taxasums %>%
#     mutate_if(is.character, str_trim)
# 
# # make proper NAs then convert to Other
# taxasums[taxasums == "NA"] <- NA
# taxasums[is.na(taxasums)] <- "Other"
# 
# # split them up so we can choose a taxa
# taxonomy <- taxasums %>% 
#     select(Kingdom:Species)
# sums <- taxasums %>% 
#     select(!Kingdom:Species)
# 
# # choose a level and select only that level
# df <- bind_cols(taxonomy[,taxa], sums)
# 
# # collapse into unique rows
# dfu <- df %>%
#     group_by_at(taxa) %>%
#     summarise_all(sum) %>%
#     data.frame()
# 
# # replacing taxa into rownames
# dfu <- column_to_rownames(dfu, var = "Genus")
# 
# # transpose and convert to proportions
# # rows are samples, columns are organisms
# # some samples may be garbage and need to be removed
# dfutp <-na.omit(as.data.frame(round(prop.table(as.matrix(t(dfu)), 1) * 100,1)))
# 
# # filter based on the threshold
# names <- colnames(dfutp)
# counter <- 0
# tracker <- 0
# for(i in apply(dfutp, 2, function(x) max(x, na.rm = TRUE))){
#     counter <- counter + 1
#     if (i < threshold){
#         names[counter] <- "Other"
#         tracker <- tracker + 1
#     }
# }
# colnames(dfutp) <- names
# Other <- as.data.frame(rowSums(dfutp[,colnames(dfutp) == "Other"])) 
# colnames(Other) <- "Other"
# All <- dfutp[,colnames(dfutp) != "Other"]
# comb <- cbind(All, Other)
# tbm  <- as.data.frame(melt(as.matrix(comb)))
# 
# # sort 
# tbm$Var2 <- factor(tbm$Var2, levels = row.names(as.table(sort(colMeans(comb)))))
# 
# lvls <- levels(tbm$Var2)
# lvls <- lvls[lvls != "Other"]
# lvls <- c("Other", lvls)
# tbm$Var2 <- factor(tbm$Var2, levels = lvls)

source("C:/Users/zjard/Documents/16S_functions.R")
tbm <- taxa_prop_table()

#---- For single soils only 
# choose just 4 soils 
tbm_f <- tbm %>%
    filter(Var1 == "A1.1" | Var1 == "A2.3" | Var1 == "A3.3" | Var1 == "A4.2")
tbm_f <- tbm_f %>%
    mutate(Var2 = str_to_sentence(Var2))

# big colourscheme
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# col_vector <- col_vector[5:length(col_vector)]

# redo this because we subsetted data
comb2 <- comb %>%
    mutate(soil = rownames(comb)) %>%
    filter(soil == "A1.1" | soil == "A2.3" | soil == "A3.3" | soil == "A4.2") %>%
    select(-soil)

colnames(comb2) <- str_to_sentence(colnames(comb2))

tbm_f$Var2 <- factor(tbm_f$Var2, levels = row.names(as.table(sort(colMeans(comb2)))))
lvls <- levels(tbm_f$Var2)
lvls <- lvls[lvls != "Other"]
lvls <- c("Other", lvls)
tbm_f$Var2 <- factor(tbm_f$Var2, levels = lvls)

# hmmm
set.seed(1)
cols <- sample(col_vector, ncol(comb))

# change their labels
tbm_f %>%
    mutate(Var1 = str_replace_all(Var1, "A1.1", "One")) %>%
    mutate(Var1 = str_replace_all(Var1, "A2.3", "Two")) %>%
    mutate(Var1 = str_replace_all(Var1, "A3.3", "Three")) %>%
    mutate(Var1 = str_replace_all(Var1, "A4.2", "Four")) %>%
    mutate(Var1 = fct_relevel(Var1, "One", "Two", "Three", "Four")) %>%
    
    ggplot( aes(fill=Var2, y=value, x=Var1)) + 
    geom_bar(position="fill", stat="identity") +
    scale_y_continuous(labels=scales::percent) +
    xlab("Soil") + ylab("Abundance (%)") + labs(fill=taxa) +
    # theme_solarized_2() + 
    # theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
    ggtitle(glue('16S Soil, Threshold: {threshold}%, Number of taxa in Other: {tracker}')) +
    # scale_fill_brewer(palette = "Paired") +
    scale_fill_manual(values = cols) +
    # scale_fill_viridis_d() +
    theme(axis.text.x = element_text(size = 14)) +
    theme_q2r() + 
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    theme(legend.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 12))


# ggplot(tbm2, aes(fill=Var2, y=value, x=date)) + 
#     geom_bar(position="fill", stat="identity", col="black") +
#     scale_y_continuous(labels=scales::percent) +
#     xlab("") + ylab("Relative frequency") + labs(fill=taxa) +
#     # theme_solarized_2() + 
#     theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
#     ggtitle(glue('16S Soil, threshhold: {threshhold}%')) +
#     scale_fill_brewer(palette = "Paired")+ 
#     scale_fill_viridis_d()  +
#     facet_grid(soil~monomer)

#RColorBrewer::display.brewer.all()

# soil diversity
metadata<-read_q2metadata("meta-data.txt")
shannon<-read_qza("shannon_vector.qza")

shannon<-shannon$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged
# library(gplots)
# gplots::venn(list(metadata=metadata$SampleID, shannon=shannon$SampleID))
metadata<-
    metadata %>% 
    left_join(shannon)
# head(metadata)

metadata %>%
    filter(!is.na(shannon_entropy)) %>%
    mutate(soil = str_to_sentence(soil)) %>%
    mutate(soil = fct_relevel(soil, "One", "Two", "Three", "Four")) %>%
    mutate(time= str_replace_all(time, "B", "A")) %>%
    mutate(time= str_replace_all(time, "C", "B")) %>%
    ggplot(aes(x = time, y = shannon_entropy, fill = time)) + 
    stat_summary(geom="bar", fun.data=mean_se, color="black") +
    geom_jitter(shape=21, width=0.2, height=0, size = 4) +
    coord_cartesian(ylim=c(2,9)) + theme_q2r() +
    theme(strip.text.x = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    ggtitle("Soil") + 
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    theme(legend.position = "none") + 
    facet_grid(~`soil`) +
    labs(y = "Shannon Diversity")

uwunifrac<-read_qza("unweighted_unifrac_pcoa_results.qza")
shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 

uwunifrac$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    left_join(shannon) %>%
    mutate(soil = fct_relevel(soil, "one", "two", "three", "four")) %>%
    mutate(time= str_replace_all(time, "B", "A")) %>%
    mutate(time= str_replace_all(time, "C", "B")) %>%
    ggplot(aes(x=PC1, y=PC2, color=`soil`, shape=`time`, size=shannon_entropy)) +
    geom_point(alpha=0.7) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() +
    scale_shape_manual(values=c(16,1), name="Time") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    scale_size_continuous(name="Shannon Diversity") +
    scale_color_discrete(name="Soil")

### Enrichments ###
# taxa = "Genus"
# threshold = 4
# 
# 
# metadata<-read_q2metadata("sample-metadata.tsv")
# SVs<-read_qza("enrichment-table.qza")$data
# taxonomy<-read_qza("enrichment-taxonomy.qza")$data %>% parse_taxonomy()
# taxasums<-summarize_taxa(SVs, taxonomy)$Species
# 
# 
# taxon <- rownames(taxasums)
# taxasums <- as_tibble(cbind(taxon, taxasums))
# 
# # separate that into a column for each taxa
# taxasums <- separate(taxasums, taxon, c("Kingdom", "Phylum", 
#                                         "Class", "Order", 
#                                         "Family", "Genus", 
#                                         "Species"), sep = ";")
# 
# # if its a string remove whitespace
# taxasums <- taxasums %>%
#     mutate_if(is.character, str_trim)
# 
# # make proper NAs then convert to Other
# taxasums[taxasums == "NA"] <- NA
# taxasums[is.na(taxasums)] <- "Other"
# 
# # split them up so we can choose a taxa
# taxonomy <- taxasums %>% 
#     select(Kingdom:Species)
# sums <- taxasums %>% 
#     select(!Kingdom:Species)
# 
# # choose a level and select only that level
# df <- as_tibble(cbind(taxonomy[,taxa], sums))
# 
# # collapse into unique rows
# dfu <- df %>%
#     group_by_at(taxa) %>%
#     summarise_all(sum) %>%
#     data.frame()
# 
# # replacing taxa into rownames
# rownames(dfu) <- dfu[,taxa]
# dfu <- dfu %>%
#     select(!taxa)
# 
# # transpose and convert to proportions
# # rows are samples, columns are organisms
# dfut <- t(dfu)
# dfutp <-as.data.frame(round(prop.table(as.matrix(dfut), 1) * 100,1))
# 
# # some samples may be garbage and need to be removed
# dfutp <- na.omit(dfutp)
# ncol(dfutp)
# # filter based on the threshold
# names <- colnames(dfutp)
# counter <- 0
# tracker <- 0
# for(i in apply(dfutp, 2, function(x) max(x, na.rm = TRUE))){
#     counter <- counter + 1
#     if (i < threshold){
#         names[counter] <- "Other"
#         tracker <- tracker + 1
#     }
# }
# colnames(dfutp) <- names
# Other <- as.data.frame(rowSums(dfutp[,colnames(dfutp) == "Other"])) 
# colnames(Other) <- "Other"
# All <- dfutp[,colnames(dfutp) != "Other"]
# comb <- cbind(All, Other)
# tbm  <- as.data.frame(melt(as.matrix(comb)))



tbm <- tbm %>%
    mutate(Var2 = str_to_sentence(Var2))

colnames(comb) <- str_to_sentence(colnames(comb))

# sort 
tbm$Var2 <- factor(tbm$Var2, levels = row.names(as.table(sort(colMeans(comb)))))

lvls <- levels(tbm$Var2)
lvls <- lvls[lvls != "Other"]
lvls <- c("Other", lvls)
tbm$Var2 <- factor(tbm$Var2, levels = lvls)


tbm2 <- separate(tbm, Var1, sep = "\\.", into = c("date", "monomer", "soil"))
tbm2$date <- factor(tbm2$date, levels = c("June21", "July7", "July20", "July29"))

tbm2 <-tbm2 %>%
    mutate(soil = str_replace(soil, "1", "One")) %>%
    mutate(soil = str_replace(soil, "2", "Two")) %>%
    mutate(soil = str_replace(soil, "3", "Three")) %>%
    mutate(soil = str_replace(soil, "4", "Four")) %>%
    mutate(soil = fct_relevel(soil, "One", "Two", "Three", "Four"))

set.seed(1)
cols <- sample(col_vector, ncol(comb))

ggplot(tbm2, aes(fill=Var2, y=value, x=date)) +
    geom_bar(position="fill", stat="identity") +
    scale_y_continuous(labels=scales::percent) +
    xlab("Time") + ylab("Abundance (%)") + labs(fill=taxa) +
    # theme_solarized_2() +
    theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
    ggtitle(glue('16S Enrichments, Threshold: {threshold}%')) +
    theme_q2r() +
    # scale_fill_brewer(palette = "Paired")+
    scale_fill_manual(values = cols) +
    # scale_fill_viridis_d()  +
    facet_grid(soil~monomer) + 
    theme(strip.text.x = element_text(size = 14)) +
    theme(strip.text.y = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(plot.title = element_text(size = 15)) + 
    theme(legend.text = element_text(size = 10)) + 
    theme(legend.title = element_text(size = 12))

metadata<-read_q2metadata("sample-metadata.tsv")
shannon<-read_qza("shannon_vector.qza")
shannon<-shannon$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged
# gplots::venn(list(metadata=metadata$SampleID, shannon=shannon$SampleID))

metadata<-
    metadata %>% 
    left_join(shannon)

metadata$date <- factor(metadata$date, levels = c("June21", "July7", "July20", "July29"))

metadata %>%
    filter(!is.na(shannon_entropy)) %>%
    mutate(soil = str_to_sentence(soil)) %>%
    mutate(soil = fct_relevel(soil, "One", "Two", "Three", "Four")) %>%
    ggplot(aes(x=date, y=shannon_entropy, color=soil, shape = treatment, group = interaction(treatment, soil)), alpha = 0.8) +
    geom_point(size = 4) +
    geom_line(linewidth = 1) +
    theme_bw() + scale_shape_manual(values=c(15,19), name="Monomer") +
    xlab("Days") +
    ylab("Shannon Diversity") + 
    scale_color_viridis_d(name="Soil") + 
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(plot.title = element_text(size = 15)) + 
    theme(legend.text = element_text(size = 10)) + 
    theme(legend.title = element_text(size = 12))

# metadata<-read_q2metadata("sample-metadata.tsv")
uwunifrac<-read_qza("unweighted_unifrac_pcoa_results.qza")
shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 

uwunifrac$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    left_join(shannon) %>%
    mutate(soil = str_to_sentence(soil)) %>%
    mutate(soil = fct_relevel(soil, "One", "Two", "Three", "Four")) %>%
    ggplot(aes(x=PC1, y=PC2, color=`soil`, shape=`treatment`, size=shannon_entropy)) +
    geom_point(alpha=0.5, stroke = 1) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() +
    scale_shape_manual(values=c(16,1), name="Monomer") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    scale_size_continuous(name="Shannon Diversity") +
    scale_color_discrete(name="Body Site") + 
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.title.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    theme(plot.title = element_text(size = 15)) + 
    theme(legend.text = element_text(size = 10)) + 
    theme(legend.title = element_text(size = 12))

