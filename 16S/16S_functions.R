library(qiime2R)
library(tidyverse)
library(reshape2)

taxa_sums <- function(table, taxonomy) {
  SVs <-read_qza(table)$data
  taxonomy <-read_qza(taxonomy)$data %>% parse_taxonomy()
  taxasums <-summarize_taxa(SVs, taxonomy)$Species
  taxasums
}

taxa_prop_table <- function(taxasums, taxa = "Genus"){
   # 
   # # metadata<-read_q2metadata("meta-data.txt")
   #  SVs <-read_qza(table)$data
   #  taxonomy <-read_qza(taxonomy)$data %>% parse_taxonomy()
   #  taxasums <-summarize_taxa(SVs, taxonomy)$Species
    
    # rownames are concatenated taxa, pull out then make it a column
    taxasums <- as_tibble(rownames_to_column(taxasums, var = "taxon"))
    
    # separate that into a column for each taxa
    taxasums <- separate(taxasums, taxon, c("Kingdom", "Phylum", 
                                            "Class", "Order", 
                                            "Family", "Genus", 
                                            "Species"), sep = ";")
    
    # if its a string remove whitespace
    taxasums <- taxasums %>%
        mutate_if(is.character, str_trim)
    
    # make proper NAs then convert to Other
    taxasums[taxasums == "NA"] <- NA
    taxasums[is.na(taxasums)] <- "Other"
    
    # split them up so we can choose a taxa
    taxonomy <- taxasums %>% 
        select(Kingdom:Species)
    sums <- taxasums %>% 
        select(!Kingdom:Species)
    
    # choose a level and select only that level
    df <- bind_cols(taxonomy[,taxa], sums)
    
    # collapse into unique rows
    dfu <- df %>%
        group_by_at(taxa) %>%
        summarise_all(sum) %>%
        data.frame()
    
    # replacing taxa into rownames
    dfu <- column_to_rownames(dfu, var = taxa)
    
    # transpose and convert to proportions
    # rows are samples, columns are organisms
    # some samples may be garbage and need to be removed
    dfutp <-na.omit(as.data.frame(round(prop.table(as.matrix(t(dfu)), 1) * 100,1)))
    dfutp
}

filter_prop_table <- function(prop_table, threshold = 2){
  
  # filter based on the threshold
  names <- colnames(prop_table)
  counter <- 0
  tracker <- 0
  for(i in apply(prop_table, 2, function(x) max(x, na.rm = TRUE))){
    counter <- counter + 1
    if (i < threshold){
      names[counter] <- "Other"
      tracker <- tracker + 1
    }
  }
  tracker <<- tracker
  
  colnames(prop_table) <- names
  Other <- as.data.frame(rowSums(prop_table[,colnames(prop_table) == "Other"])) 
  colnames(Other) <- "Other"
  All <- prop_table[,colnames(prop_table) != "Other"]
  comb <- cbind(All, Other)
  comb
  
}


filter_samples <- function(smp_filt, table, table_melt) {
    
    smp_filt <- smp_filt %>% paste(collapse = "|")
    
    tbl_f <- table %>%
        rownames_to_column(var = "smp") %>%
        filter(str_detect(smp, smp_filt)) %>%
        column_to_rownames(var = "smp")
    colnames(tbl_f) <- str_to_sentence(colnames(tbl_f))
    
    tbm_f <- table_melt %>%
        mutate(Var2 = str_to_sentence(Var2)) %>% 
        filter(str_detect(Var1, smp_filt))
    
    tbm_f$Var2 <- factor(tbm_f$Var2, levels = row.names(as.table(sort(colMeans(tbl_f)))))
    
    lvls <- levels(tbm_f$Var2)
    lvls <- lvls[lvls != "Other"]
    lvls <- c("Other", lvls)
    tbm_f$Var2 <- factor(tbm_f$Var2, levels = lvls)
    tbm_f
    
}
   
library(RColorBrewer)

lrg_colors <- function(smp, seed) {
  n <- 60
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  set.seed(seed)
  cols <- sample(col_vector, smp)
  cols
}
