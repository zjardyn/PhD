library(readxl)
library(tidyverse)
library(viridisLite)
library(reshape2)

dat <- read_xlsx("feature-table-16S-taxonomy-repset.xlsx")
dat2 <- read_xlsx("feature-table-16S-taxonomy-repset2.xlsx")

ProcessData <- function(df = dat, df2 = dat2, taxa = "Family", threshold = 10) {
    
    df1_counts <- df %>% select(colnames(df)[3:(ncol(df)-8)])
    df2_counts <- df2 %>% select(colnames(df2)[10:(ncol(df2)-8)])
    df1_taxa <- df %>% select(colnames(df)[21:27])
    df2_taxa <- df2 %>% select(colnames(df2)[46:52])
    colnames(df1_taxa) <- str_to_sentence(colnames(df1_taxa))
    colnames(df2_taxa)[1] <- "Domain"
    
    dfo1 <- cbind(df1_counts, df1_taxa[taxa])
    dfo2 <- cbind(df2_counts, df2_taxa[taxa])
    
    dfo1[is.na(dfo1)] <- "Other"
    dfo2[is.na(dfo2)] <- "Other"
    
    dfo1_u <- dfo1 %>%
        group_by_at(taxa) %>%
        summarise_all(sum) %>%
        data.frame()
    
    dfo2_u <- dfo2 %>%
        group_by_at(taxa) %>%
        summarise_all(sum) %>%
        data.frame()
    
    dfa_u <- merge(dfo1_u, dfo2_u, by = taxa, all = TRUE)
    dfa_u[is.na(dfa_u)] <- 0
    
    S1 <- dfa_u$A1.1 + dfa_u$A1.2 + dfa_u$B1.1 + dfa_u$B1.2 + dfa_u$B1.3
    S2 <- dfa_u$A2.3 
    S3 <- dfa_u$A3.1 + dfa_u$A3.2 + dfa_u$A3.3 + dfa_u$B3.1 + dfa_u$B3.2 + dfa_u$B3.3
    S4 <- dfa_u$A4.1 + dfa_u$A4.2 + dfa_u$A4.3 + dfa_u$B4.1 + dfa_u$B4.2 + dfa_u$B4.3
    
    dfa_u <- cbind(dfa_u[,1] ,June10.Styrene.1 = S1, June10.Styrene.2 = S2, June10.Styrene.3 = S3, June10.Styrene.4 = S4, June10.Hexadecane.1 = S1, June10.Hexadecane.2 = S2, June10.Hexadecane.3 = S3, June10.Hexadecane.4 = S4, dfa_u[,20:ncol(dfa_u)])
    colnames(dfa_u)[1] <- taxa
    
    rownames(dfa_u) <- dfa_u[,1]
    dfa_u <- dfa_u %>%
        select(! taxa)
    
    dfa_u_t <- t(dfa_u)
    dfo_u_t_prop <-as.data.frame(round(prop.table(as.matrix(dfa_u_t), 1) * 100,1))
    dfo_u_t_prop <- na.omit(dfo_u_t_prop)
    dfo_sub <- dfo_u_t_prop[,apply(dfo_u_t_prop, 2, function(x) max(x, na.rm = TRUE))>threshold]
    tbm <- as.data.frame(melt(as.matrix(dfo_sub)))
    tbm$Var1 <- str_replace_all(tbm$Var1, "^S\\.", "S")
    tbm1 <- cbind(as.data.frame(str_split_fixed(tbm$Var1, "\\.", 3)), Var2 = tbm$Var2, value = tbm$value)
    tbm1$V1 <- tbm1$V1 %>% str_replace("June10", "Soil_June10") 
    tbm1$V1 <- tbm1$V1 %>% str_replace("^SJ", "Subculture_J")
    tbm1$Var2 <- factor(tbm$Var2, levels = row.names(as.table(sort(colMeans(dfo_sub)))))
    tbm1$V1 <- factor(tbm1$V1, levels = c("Soil_June10", "June21", "July7", "July20", "July29", "Subculture_July29"))
    taxa <<- taxa
    threshold <<- threshold
    return(tbm1)
}

tbm1 <- ProcessData()

ggplot(tbm1, aes(fill=Var2, y=value, x=V1)) + 
    geom_bar(position="fill", stat="identity", col="grey50") +
    scale_y_continuous(labels=scales::percent) +
    xlab("") + ylab("Relative frequency") + labs(fill=taxa) +
    theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
    ggtitle(paste("16S Soil Enrichments, Threshold: > ", threshold, sep = "")) + 
    facet_grid(V2 ~ V3)

ggplot(tbm1, aes(x = V1, y = Var2,size = value,fill=value), colsep=c(1:100), rowsep=(1:100), sepwidth=c(5,1)) + geom_point(shape = 21, alpha=0.4) + ggtitle("") + xlab("") + ylab("") + theme(axis.text = element_text(colour= "black", size = 12), text = element_text(size=15), axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))+ scale_size_area(max_size = 15,guide="none") + labs(fill="Relative\nfrequency (%)") +
    ggtitle(paste("16S Soil Enrichments, Taxa: ", taxa, ", Threshold: ", threshold, sep = "")) + 
    facet_grid(V2 ~ V3) +
    scale_fill_viridis_c()

        
df <- read_xlsx("feature-table-16S-taxonomy-repset.xlsx")
df2 <- read_xlsx("feature-table-16S-taxonomy-repset2.xlsx")

df1_counts <- df %>% select(colnames(df)[3:(ncol(df)-8)])
df2_counts <- df2 %>% select(colnames(df2)[10:(ncol(df2)-8)])
df1_taxa <- df %>% select(colnames(df)[21:27])
df2_taxa <- df2 %>% select(colnames(df2)[46:52])
colnames(df1_taxa) <- str_to_sentence(colnames(df1_taxa))
colnames(df2_taxa)[1] <- "Domain"

taxa <- "Species"

if(taxa == "Species" || taxa == "Genus"){
    print("yes")
}

dfo1 <- cbind(df1_counts, df1_taxa[taxa])
dfo2 <- cbind(df2_counts, df2_taxa[taxa])

dfo1[is.na(dfo1)] <- "Other"
dfo2[is.na(dfo2)] <- "Other"

dfo1_u <- dfo1 %>%
    group_by_at(taxa) %>%
    summarise_all(sum) %>%
    data.frame()

dfo2_u <- dfo2 %>%
    group_by_at(taxa) %>%
    summarise_all(sum) %>%
    data.frame()

dfa_u <- merge(dfo1_u, dfo2_u, by = taxa, all = TRUE)
dfa_u[is.na(dfa_u)] <- 0

# Sum initial samples 
S1 <- dfa_u$A1.1 + dfa_u$A1.2 + dfa_u$B1.1 + dfa_u$B1.2 + dfa_u$B1.3
S2 <- dfa_u$A2.3 
S3 <- dfa_u$A3.1 + dfa_u$A3.2 + dfa_u$A3.3 + dfa_u$B3.1 + dfa_u$B3.2 + dfa_u$B3.3
S4 <- dfa_u$A4.1 + dfa_u$A4.2 + dfa_u$A4.3 + dfa_u$B4.1 + dfa_u$B4.2 + dfa_u$B4.3

dfa_u <- cbind(dfa_u[,1] ,June10.Styrene.1 = S1, June10.Styrene.2 = S2, June10.Styrene.3 = S3, June10.Styrene.4 = S4, June10.Hexadecane.1 = S1, June10.Hexadecane.2 = S2, June10.Hexadecane.3 = S3, June10.Hexadecane.4 = S4, dfa_u[,20:ncol(dfa_u)])
colnames(dfa_u)[1] <- taxa

rownames(dfa_u) <- dfa_u[,1]
dfa_u <- dfa_u %>%
    select(! taxa)

dfa_u_t <- t(dfa_u)
dfo_u_t_prop <-as.data.frame(round(prop.table(as.matrix(dfa_u_t), 1) * 100,1))
# dfo_u_t_prop <-as.data.frame((prop.table(as.matrix(dfa_u_t), 1)* 100))
# View(dfo_u_t_prop)
# remove the NaN row
dfo_u_t_prop <- na.omit(dfo_u_t_prop)

#Choose a selection of taxa with a % > 3 (Note: might have to play around with this until you get a reasonable number of taxa to display)
dfo_sub <- dfo_u_t_prop[,apply(dfo_u_t_prop, 2, function(x) max(x, na.rm = TRUE))>10]

# melt the dataset
tbm <- as.data.frame(melt(as.matrix(dfo_sub)))

tbm$Var1 <- str_replace_all(tbm$Var1, "^S\\.", "S")

tbm1 <- cbind(as.data.frame(str_split_fixed(tbm$Var1, "\\.", 3)), Var2 = tbm$Var2, value = tbm$value)

tbm1$V1 <- tbm1$V1 %>% str_replace("June10", "Soil_June10") 
tbm1$V1 <- tbm1$V1 %>% str_replace("^SJ", "Subculture_J")
#Set the order of the taxa on the plot (Note: optional)
tbm1$Var2 <- factor(tbm$Var2, levels = row.names(as.table(sort(colMeans(dfo_sub)))))
tbm1$V1 <- factor(tbm1$V1, levels = c("Soil_June10", "June21", "July7", "July20", "July29", "Subculture_July29"))
# tbm$Var1 %>% str_replace_all("^A", "hello")

ggplot(tbm1, aes(fill=Var2, y=value, x=V1)) + 
    geom_bar(position="fill", stat="identity", col="grey50") +
    scale_y_continuous(labels=scales::percent) +
    xlab("") + ylab("Relative frequency") + labs(fill=taxa) +
    theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
    ggtitle("16S Amplicon Sequencing of Landfill Soil") + 
    facet_grid(V2 ~ V3)


ggplot(tbm1, aes(x = V1, y = Var2,size = value,fill=value), colsep=c(1:100), rowsep=(1:100), sepwidth=c(5,1)) + geom_point(shape = 21, alpha=0.4) + ggtitle("") + xlab("") + ylab("") + theme(axis.text = element_text(colour= "black", size = 12), text = element_text(size=15), axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))+ scale_size_area(max_size = 15,guide="none") + labs(fill="Relative\nfrequency (%)") +
    ggtitle(paste("16S Amplicon Sequencing of Landfill Soil, Taxa: ", taxa, sep = "")) + 
    facet_grid(V2 ~ V3) +
    scale_fill_viridis_c()