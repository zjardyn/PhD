iris2 <- iris %>% 
    select(Sepal.Length:Petal.Length)

colnames(iris2) <- c("x", "y", "z")

ggplot(iris2, aes(x,y, size = z)) + 
    geom_point() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank(),
          axis.title=element_text(size=25,face="bold"),
          legend.title = element_text(size = 25, face = "bold",),
          legend.text = element_blank())

iris4 <- iris %>% 
    select(Petal.Length:Species) 

colnames(iris4) <- c("x", "y", "z")

ggplot(iris4, aes(x,y, colour = z)) + 
    geom_point() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),
          axis.title=element_text(size=25,face="bold"),
          legend.title = element_text(size = 25, face = "bold",),
          legend.text = element_blank())
