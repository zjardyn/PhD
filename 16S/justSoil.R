library(readxl)
library(tidyverse)
library(viridisLite)
library(reshape2)

df <- read_xlsx("feature-table-16S-taxonomy-repset.xlsx")
df1_counts <- df %>% select(colnames(df)[3:(ncol(df)-8)])
df1_taxa <- df %>% select(colnames(df)[21:27])
colnames(df1_taxa) <- str_to_sentence(colnames(df1_taxa))

taxa <- "Order"

dfo1 <- cbind(df1_counts, df1_taxa[taxa])

dfo1[is.na(dfo1)] <- "Other"

dfa_u <- dfo1 %>%
    group_by_at(taxa) %>%
    summarise_all(sum) %>%
    data.frame()

rownames(dfa_u) <- dfa_u[,1]
dfa_u <- dfa_u %>%
    select(! taxa)

dfa_u_t <- t(dfa_u)
dfo_u_t_prop <-as.data.frame(round(prop.table(as.matrix(dfa_u_t), 1) * 100,1))
dfo_u_t_prop <- na.omit(dfo_u_t_prop)
dfo_sub <- dfo_u_t_prop[,apply(dfo_u_t_prop, 2, function(x) max(x, na.rm = TRUE))>4]
tbm <- as.data.frame(melt(as.matrix(dfo_sub)))
tbm$Var2 <- factor(tbm$Var2, levels = row.names(as.table(sort(colMeans(dfo_sub)))))

ggplot(tbm, aes(fill=Var2, y=value, x=Var1)) + 
    geom_bar(position="fill", stat="identity", col="grey50") +
    scale_y_continuous(labels=scales::percent) +
    xlab("") + ylab("Relative frequency") + labs(fill=taxa) +
    theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
    ggtitle("16S Amplicon Sequencing of Landfill Soil") 


ggplot(tbm1, aes(x = V1, y = Var2,size = value,fill=value), colsep=c(1:100), rowsep=(1:100), sepwidth=c(5,1)) + geom_point(shape = 21, alpha=0.4) + ggtitle("") + xlab("") + ylab("") + theme(axis.text = element_text(colour= "black", size = 12), text = element_text(size=15), axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))+ scale_size_area(max_size = 15,guide="none") + labs(fill="Relative\nfrequency (%)") +
    ggtitle(paste("16S Amplicon Sequencing of Landfill Soil, Taxa: ", taxa, sep = "")) + 
    facet_grid(V2 ~ V3) +
    scale_fill_viridis_c()
