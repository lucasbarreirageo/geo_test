install.packages("bibliometrix")
install.packages("openxlsx")

library(bibliometrixData)
library(openxlsx)
library(tidyverse)

setwd("C:/Users/Lucas/Downloads")
getwd()
dir()
W <- convert2df("savedrecs.txt", dbsource = "wos", format = "plaintext")

results <- biblioAnalysis(W, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
S
plot(x = results, k = 10, pause = FALSE)



biblioshiny()

# Create a country collaboration network
M <- metaTagExtraction(W, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net = networkPlot(NetMatrix, n = dim(NetMatrix)[1], 
                  Title = "Country Collaboration", type = "auto", 
                  size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")

A <- cocMatrix(W, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:10]

# Other networks possible

# Citation network
C <- cocMatrix(W, Field = "CR", sep = ".  ")
sort(Matrix::colSums(C), decreasing = TRUE)[1:5]

# Author network
AU <- cocMatrix(W, Field = "AU", sep = ";")
sort(Matrix::colSums(AU), decreasing = TRUE)[1:5]

# Author keyword network
AK <- cocMatrix(M, Field = "DE", sep = ";")
sort(Matrix::colSums(AK), decreasing = TRUE)[1:5]

# Keyword Plus network
AKP <- cocMatrix(W, Field = "ID", sep = ";")
sort(Matrix::colSums(AKP), decreasing = TRUE)[1:5]

# The following code calculates a classical article coupling network:


NetMatrix <- biblioNetwork(W, analysis = "coupling", network = "authors", sep = ";")
NetMatrix <- biblioNetwork(W, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, normalize = "salton", weighted=NULL, n = 10, 
                Title = "references' co-citation", type = "fruchterman", 
                size=5, size.cex=T, remove.multiple=TRUE, labelsize=0.8, label.n=10, label.cex=F)


# Create a co-citation network 
NetMatrix <- biblioNetwork(W, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
net = networkPlot(NetMatrix, n = 10, 
                  Title = "Co-Citation Network", type = "fruchterman", 
                  size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
# keywords
NetMatrix <- biblioNetwork(W, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net = networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, 
                  Title = "Keyword Co-occurrences", type = "fruchterman", 
                  size=T, edgesize = 5, labelsize=0.7)
# Collaboration networks (authors)

NetMatrix <- biblioNetwork(W, analysis = "collaboration", network = "authors", sep = ";")

# Plot the network
net = networkPlot(NetMatrix, n = 10, 
                  Title = "Collaboration Network authors", type = "auto", 
                  size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
# Conceptual Structure using keywords (method="MCA")
CS <- conceptualStructure(W,field="DE", method="MCA", minDegree=5, clust=2, stemming=FALSE, labelsize=10, documents=10)
