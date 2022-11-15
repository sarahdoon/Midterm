# Problem 1
# put file into variable
Mcountdata <- read.csv("/Users/sarahferaidoon/Desktop/Mnemiopsis_count_data.csv")
Mcountdata

Mcoldata <- read.csv("/Users/sarahferaidoon/Desktop/Mnemiopsis_col_data.csv")
Mcoldata

Mcountdata$avg <- rowMeans(Mcountdata[,2:9])
topgenes <- Mcountdata[order(Mcountdata$avg,decreasing = TRUE), 1]
head(topgenes,5)
# >[1] "ML20395a"  "ML26358a"  "ML46651a"  "ML020045a" "ML00017a" 
# ML20395a has molecular function: nucleotide binding, translation elongation factor activity, GTPase activity, GTP binding; Cellular Component: ribosome; Biologica process: nematode larval development, gamete generation, positive regulation of growth rate, locomotion, embryo development ending in birth or egg hatching, regulation of translational elongation 
# ML26358a has molecular function : nucleotide binding, ATPbinding; Cellular Componend: cytoskeleton, cytoplasm; Biological Process: cytoskeleton organization, actin cytoskeleton organization, cytokinesis, embryo development ending in birth or egg hatching
# ML46651a has cellular component: membrane attack complex
# ML020045a molecular function: nucleotide binding, GTPase activity, GTP binding; Cellular Component: microtubule, cytoskeleton, cytoplasm, protein complex; Biological process: microtubule-based process, microtubule-based movement, protein polymerization
# ML00017a

# Problem 2
# Yes, the top 5 genes will be different if done on a per column basis
# aboral1
head(Mcountdata[order(Mcountdata$aboral1,decreasing=TRUE),1],5)
# [1] "ML46651a"  "ML20395a"  "ML020045a" "ML174731a" "ML26358a" 
# ML174731a is new, missing ML00017a

# aboral2
head(Mcountdata[order(Mcountdata$aboral2,decreasing=TRUE),1],5)
# [1] "ML20395a"  "ML46651a"  "ML26358a"  "ML01482a"  "ML034334a"
# "ML01482a" & "ML034334a" are new, missing ML00017a and ML020045a

# aboral3
head(Mcountdata[order(Mcountdata$aboral3,decreasing=TRUE),1],5)
# [1] "ML20395a"  "ML01482a"  "ML26358a"  "ML46651a"  "ML034334a"
# "ML01482a" & "ML034334a" are new

# aboral4
head(Mcountdata[order(Mcountdata$aboral4,decreasing=TRUE),1],5)
# [1] "ML01482a"  "ML20395a"  "ML034334a" "ML46651a"  "ML034336a"
# "ML01482a" & "ML034334a" & "ML034336a" are new

# oral1
head(Mcountdata[order(Mcountdata$oral1,decreasing=TRUE),1],5)
# [1] "ML20395a"  "ML020045a" "ML04011a"  "ML26358a"  "ML00017a" 
# ML04011a is new

# oral2
head(Mcountdata[order(Mcountdata$oral2,decreasing=TRUE),1],5)
# [1] "ML20395a"  "ML020045a" "ML04011a"  "ML00017a"  "ML26358a" 
# ML04011a is new

# oral3
head(Mcountdata[order(Mcountdata$oral3,decreasing=TRUE),1],5)
# [1] "ML20395a"  "ML004510a" "ML26358a"  "ML00017a"  "ML04011a" 
# "ML004510a" is new

# oral4
head(Mcountdata[order(Mcountdata$oral4,decreasing=TRUE),1],5)
# [1] "ML20395a"  "ML004510a" "ML46651a"  "ML020045a" "ML00017a"
# "ML004510a" is new

# Problem 3
# from columns 2-9 in Mcountdata, put the colmeans into a list
MCDmean <- c(colMeans(Mcountdata[,2:9]))
MCDmean
# apply sd method to columns 2 - 9 of Mcountdata
MCDsd <- sapply(Mcountdata[,2:9],sd)
MCDsd

# put all columns in Mcountdata in variable
scale <- Mcountdata[,1:9]
for (i in 2:9){# for every item in the column
  c <- (MCDmean[1])/(MCDmean[i-1]) # create scaling factor of meancol/meancolumn-1
  scale[,i]<- c * scale[i] # multiply column by this scaling factor
}
scale
colMeans(scale[,2:9])

# Problem 4
cor(scale[,2:9])
# aboral-aboral and oral-oral correlations are closer than aboral-oral


# Problem 5
# I would break the data into thirds somehow

# Problem 6
# create a dataframe with gene names
vardf <- data.frame()
for (i in 1:length(scale$Gene)){ # for each Gene item in the length of the scaled data
  if(sum(scale[i,2:9])>=0.5){
    rbind(vardf,c(scale[i,1],var(as.numeric(scale[i,2:9]))))
  }
}
vardf

# Problem 7
