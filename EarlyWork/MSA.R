library(Biostrings)
library(muscle)
library(DECIPHER)
library(msa)
library(ape)
library(ggtree)
library(tidyverse)
library(seqinr)

setwd("~/OneDrive - University of Waterloo/PhD/Genes")
dat <- readAAStringSet("seqdump.txt")
#node.labels <- read.delim('labels.txt', sep = '\n', header = F)

# better names
names(dat@ranges) <- node.labels$V1
names(dat@ranges)
# multi-sequence alignments
msa1 <- msa::msa(dat)
msa2 <- muscle::muscle(dat)

msa1.bin <- as.AAbin(msa2)

# workflow 1
tree <- dist.aa(msa1.bin, pairwise.deletion = T)
tree1 <- nj(tree)
# tree2 <-boot.phylo(tree1, msa2)
tree1$tip.label
names(dat@ranges)

ggtree(tree1) + 
    geom_tiplab(size = 3) +
    ggplot2::xlim(0, 300) 

msaplot(tree1, 'out.fasta')


alignment2Fasta <- function(alignment, filename) {
    sink(filename)
    
    n <- length(rownames(alignment))
    for(i in seq(1, n)) {
        cat(paste0('>', rownames(alignment)[i]))
        cat('\n')
        the.sequence <- toString(unmasked(alignment)[[i]])
        cat(the.sequence)
        cat('\n')  
    }
    
    sink(NULL)
}

alignment2Fasta(msa2, 'out.fasta')
