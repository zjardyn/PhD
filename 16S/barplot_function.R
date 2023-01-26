library(qiime2R)
library(tidyverse)
library(reshape2)

taxa_prop_table <- function(table = "soil-table.qza", taxonomy ="taxonomy.qza", taxa = "Genus", threshold = 2){
    
    # metadata<-read_q2metadata("meta-data.txt")
    SVs <-read_qza(table)$data
    taxonomy <-read_qza(taxonomy)$data %>% parse_taxonomy()
    taxasums <-summarize_taxa(SVs, taxonomy)$Species
    
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
    dfu <- column_to_rownames(dfu, var = "Genus")
    
    # transpose and convert to proportions
    # rows are samples, columns are organisms
    # some samples may be garbage and need to be removed
    dfutp <-na.omit(as.data.frame(round(prop.table(as.matrix(t(dfu)), 1) * 100,1)))
    
    # filter based on the threshold
    names <- colnames(dfutp)
    counter <- 0
    tracker <- 0
    for(i in apply(dfutp, 2, function(x) max(x, na.rm = TRUE))){
        counter <- counter + 1
        if (i < threshold){
            names[counter] <- "Other"
            tracker <- tracker + 1
        }
    }
    colnames(dfutp) <- names
    Other <- as.data.frame(rowSums(dfutp[,colnames(dfutp) == "Other"])) 
    colnames(Other) <- "Other"
    All <- dfutp[,colnames(dfutp) != "Other"]
    comb <- cbind(All, Other)
    # comb
    # tbm  <- as.data.frame(melt(as.matrix(comb)))
    # tbm
}
