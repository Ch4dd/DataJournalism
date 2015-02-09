###Heatmapppping
#install.packages("RColorBrewer")
#install.packages("gplots")
library(RColorBrewer)
library(gplots)
library("lattice")
load("C:/Users/Chadd/Desktop/based/2_1.RData")
?lattice
## WE makin DFs
load
Positivity<-ratio
LexDiversity<-diverse
Offensiveness<-scores
Based<-basedcos
###
Positivity<-(Positivity -min(Positivity))/(max(Positivity)-min(Positivity))
#Based<-(Based -min(Based))/(max(Based)-min(Based))
LexDiversity<-(LexDiversity -min(LexDiversity))/(max(LexDiversity)-min(LexDiversity))
Offensiveness<-(Offensiveness -min(Offensiveness))/(max(Offensiveness)-min(Offensiveness))

ayyy<-data.frame(Positivity, accounts, LexDiversity, Offensiveness)

bae<-data.frame( names(Based), Based)
names(bae)[1] <- "accounts"
df<-merge(ayyy, bae, "accounts")

row.names(df)<-df$accounts

df<-df[-1]

?sort

###makin seperate heatmaps
posDF<-df[ order(df[,1]), ]
lexDF<-df[2]
offDF<-df[3]

scaled.df<-scale(df)
mat<-data.matrix(df)

posmat<-mat[order(mat[,1], mat[,4]), ]
lexmat<-mat[order(mat[,2], mat[,4]), ]
offmat<-mat[order(mat[,3], mat[,4]), ]
basedmat<-mat[order(mat[,4], mat[,1]), ]
basedmat

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
my_palette <- colorRampPalette(c("white", "red"))(n = 100)
jGraysFun <- colorRampPalette(brewer.pal(n = 9, "Greys"))
jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds
YlGn YlGnBu YlOrBr YlOrRd
YlGnBu <- colorRampPalette(brewer.pal(n = 9, "YlGnBu"))
oranges <- colorRampPalette(brewer.pal(n = 9, "Oranges"))
oranges <- colorRampPalette(brewer.pal(n = 9, "Greens"))
BG<- colorRampPalette(brewer.pal(n = 9, "YlOrBr"))
# creates a 5 x 5 inch image
png("based.png",    # create PNG for the heat map        
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size


heatmap.2(basedmat,
            # same data set for cell labels
          main = "Based", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(1,9),     # widens margins around plot
          col=oranges,       # use on color palette defined earlier 
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Colv=NA, Rowv=NA,
          adjRow = c(0,NA),
          adjCol = c(3,0),
          offsetRow = 0.1,
          offsetCol = 0.1, symm=TRUE, keysize = 2,
          key.xlab = "Normalized Value",
          key.xtickfun = NULL, xlab="Positivity    Lexical Diversity  Offensiveness   Based"
          )            # turn off column clustering
dev.off()               # close the PNG device

?heatmap.2
?reorderfun
###Solved the problem where I named things wrong, need to sort and create different heat maps



###The above is better, just need to get the scaling right for maximum contrast
###Rearranging by each different value: Pos
png("Pos.png",    # create PNG for the heat map        
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size


heatmap.2(posmat,
          # same data set for cell labels
          main = "Positivity", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(1,9),     # widens margins around plot
          col=jBuPuFun,       # use on color palette defined earlier 
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Colv=NA, Rowv=NA,
          adjRow = c(0,NA),
          adjCol = c(2,0),
          offsetRow = 0.1,
          offsetCol = 0.1, symm=TRUE, keysize = 2,
          key.xlab = "Normalized Value",
          key.xtickfun = NULL, 
         xlab="Positivity    Lexical Diversity  Offensiveness   Based")            # turn off column clustering
dev.off()      



###Rearranging by each different value: Lex
png("LexDiv.png",    # create PNG for the heat map        
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size


heatmap.2(lexmat,
          # same data set for cell labels
          main = "Lexical Diversity", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(1,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier 
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Colv=NA, Rowv=NA,
          adjRow = c(0,NA),
          adjCol = c(2,0),
          offsetRow = 0.1,
          offsetCol = 0.1, symm=TRUE, keysize = 2,
          key.xlab = "Normalized Value",
          key.xtickfun = NULL, 
          xlab="Positivity    Lexical Diversity  Offensiveness   Based")            # turn off column clustering
dev.off()     


###Rearranging by each different value: Lex
png("Offensive.png",    # create PNG for the heat map        
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size


heatmap.2(offmat,
          # same data set for cell labels
          main = "Offensiveness", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(1,9),     # widens margins around plot
          col=BG,       # use on color palette defined earlier 
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Colv=NA, Rowv=NA,
          adjRow = c(0,NA),
          adjCol = c(2,0),
          offsetRow = 0.1,
          offsetCol = 0.1, symm=TRUE, keysize = 2,
          key.xlab = "Normalized Value",
          key.xtickfun = NULL, 
          xlab="Positivity    Lexical Diversity  Offensiveness   Based")            # turn off column clustering
dev.off()    