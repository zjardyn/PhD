library(readxl)
library(tidyverse)
library(viridisLite)
library(reshape2)

df <- read_xlsx("feature-table-16S-taxonomy-repset.xlsx")
df2 <- read_xlsx("feature-table-16S-taxonomy-repset2.xlsx")

df1_counts <- df %>% select(colnames(df)[3:(ncol(df)-8)])
df2_counts <- df2 %>% select(colnames(df2)[10:(ncol(df2)-8)])
df1_taxa <- df %>% select(colnames(df)[21:27])
df2_taxa <- df2 %>% select(colnames(df2)[46:52])
colnames(df1_taxa) <- str_to_sentence(colnames(df1_taxa))
colnames(df2_taxa)[1] <- "Domain"

taxa <- "Species"

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



# HMMM

process_16S <- function(df, taxa = "Order", filt = 3){
    
    df[is.na(df)] <- "Other"
    df_n <- df %>% select(!Domain:Species)
    df_t <- cbind(df_n, df[taxa])
    
    df_s <- df_t %>%
        group_by_at(taxa) %>%
        summarise_all(sum) %>%
        data.frame()
    
    rownames(df_s) <- df_s[,1]
    df_s <- df_s %>%
        select(! taxa)
    
    df_s_t <- t(df_s)
    df_p <- as.data.frame((prop.table(as.matrix(df_s_t), 1)* 100))
    df_p <- na.omit(df_p)
    df_f <- df_p[, apply(df_p, 2, function(x) max(x, na.rm = TRUE))>filt]
    
    tbm <- as.data.frame(melt(as.matrix(df_f)))
    # tbm$Var2 <- factor(tbm$Var2, levels = row.names(as.table(sort(colMeans(df_f)))))
    return(tbm)
    
}

t1 <- process_16S(df = dfo1)
t2 <- process_16S(df = dfo2)


ggplot(t3, aes(fill=Var2, y=value, x=Var1)) + 
    geom_bar(position="fill", stat="identity", col="grey50") +
    scale_y_continuous(labels=scales::percent) +
    xlab("") + ylab("Relative frequency") + labs(fill="Order") +
    theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
    ggtitle("16S Amplicon Sequencing of Landfill Soil") + 
    scale_fill_viridis_d()

# Separate data into...
# counts
df_counts <- df %>%
    select(`16S-C1`:`B4-3`)

# taxa
df_taxa <- df %>%
    select(domain:species)
# and metadata
df_met <- df %>% 
    select(OTU, rep_sequence)

# unique(df_taxa$class)
df_taxa$class

# bind order to the counts and change NAs to other 
dfo <- as.data.frame(cbind(df_counts, taxa = df_taxa$order))
dfo$taxa[is.na(dfo$taxa)] <- "Other"

# Aggregate duplicate rows across samples 
dfo_unique <- dfo %>%
    group_by(taxa) %>%
    summarise_all(sum) %>%
    data.frame() 

# View(dfo_unique)
# sanity check 
ind <- dfo$taxa == "Acholeplasmatales"
sum(dfo[ind, 2])

# just changes the order to become rownames.. 
dfo_u <-dfo_unique %>%
    select(`X16S.C1`:`B4.3`)
rownames(dfo_u) <- dfo_unique$taxa
# View(dfo_u)

# transpose
dfo_u_t <- t(dfo_u)
# change to proportions (why round?)
dfo_u_t_prop <-as.data.frame(round(prop.table(as.matrix(dfo_u_t), 1) * 100,1))
dfo_u_t_prop <-as.data.frame((prop.table(as.matrix(dfo_u_t), 1)* 100))
# View(dfo_u_t_prop)
# remove the NaN row
dfo_u_t_propr <- dfo_u_t_prop[-6,]

rownames(dfo_u_t_propr)
dfo_u_t_propr <- dfo_u_t_propr[-1,]
View(dfo_u_t_propr)
#Choose a selection of taxa with a % > 3 (Note: might have to play around with this until you get a reasonable number of taxa to display)
dfo_sub <- dfo_u_t_propr[,apply(dfo_u_t_propr, 2, function(x) max(x, na.rm = TRUE))>3]

# melt the dataset
tbm <- as.data.frame(melt(as.matrix(dfo_sub)))

#Set the order of the taxa on the plot (Note: optional)
tbm$Var2 <- factor(tbm$Var2, levels = row.names(as.table(sort(colMeans(dfo_sub)))))

ggplot(tbm, aes(fill=Var2, y=value, x=Var1)) + 
    geom_bar(position="fill", stat="identity", col="grey50") +
    scale_y_continuous(labels=scales::percent) +
    xlab("") + ylab("Relative frequency") + labs(fill="Order") +
    theme(axis.text.x= element_text(angle = 90, hjust = 1)) +
    ggtitle("16S Amplicon Sequencing of Landfill Soil") + 
    scale_fill_viridis_d()

#Turn 0s into NAs
tbm[tbm == 0] <- NA

ggplot(tbm, aes(Var1,Var2,size = value,fill=value), colsep=c(1:100), rowsep=(1:100), sepwidth=c(5,1)) + geom_point(shape = 21, alpha=0.4) + ggtitle("") + xlab("") + ylab("") + theme(axis.text = element_text(colour= "black", size = 12), text = element_text(size=15), axis.text.x=element_text(angle=90, vjust = 0.5, hjust = 1))+ scale_size_area(max_size = 15,guide="none") + labs(fill="Relative\nfrequency (%)") +
    ggtitle("16S Amplicon Sequencing of Landfill Soil") + 
    scale_fill_viridis_c()
