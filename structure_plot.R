## Make pretty STRUCTURE plot(s) in R

## Set skip= and nrows= to read in data (no header) from "Inferred ancestry of individuals" section
read.table("ResultsK2.01_f",skip=55,nrows=48,header=F,stringsAsFactors=F,row.names=1) -> s
## Remove ":" column
s[,-3] -> s
## Remove parentheses from missing data column
gsub("\\(", "", s[,"V3"]) -> s[,"V3"]
gsub("\\)", "", s[,"V3"]) -> s[,"V3"]
as.numeric(s[,"V3"]) -> s[,"V3"]
c("indiv","pmd","c1","c2") -> colnames(s)
s[,c(1,2)] -> md
## Remove missing data column, set rownames=sample IDs, remove sample IDs column
s[,-2] -> s
s[,1] -> rownames(s)
s[,-1] -> s
## Resulting data frame contains N rows, K columns, rownames=sample IDs
s

## Stacked bar plot grouped by locality
pdf("structure_locality.pdf",12,6)
barplot(as.table(t(s)),xaxt="n",space=0,col=c("darkred","deepskyblue"),main="STRUCTURE (K=2) grouped by locality")
axis(1,at=(1:48)-.5,labels=rownames(s),lwd=0,lwd.ticks=1,las=2)
abline(v=seq(4,44,by=4),lwd=2,col="black") # Lines to separate my localities (line optional)
dev.off()

# Get data on haplotype and rough latitude
read.csv("crow_data.csv",header=T,stringsAsFactors=F) -> cd

# Stacked bar plot sorted by c1 and labeled with haplotype
pdf("structure_sorted_haplotype.pdf",12,6)
ss <- s[order(s$c1),]
cd[match(rownames(ss),cd$id),] -> cds
barplot(as.table(t(ss)),xaxt="n",space=0,col=c("darkred","deepskyblue"),main="STRUCTURE (K=2) sorted by proportions with (A)merican and (N)orthwestern mtDNA haplotypes")
axis(1,at=(1:48)-.5,labels=rownames(ss),lwd=0,lwd.ticks=1,las=2)
y <- rep(.03,48)
.07 -> y[cds$haplotype=="N"]
text((1:48)-.5,y,cds$haplotype,col="white")
dev.off()

pdf("structure_latitude.pdf",6,6)
## Plot structure proportions vs. latitude
plot(cd$latitude,s$c1,xlab="rough latitude",ylab="cluster 1 proportion",main="STRUCTURE (K=2) vs. latitude")
reg <- lm(s$c1~cd$latitude)
summary(reg)
abline(reg)
dev.off()

# Plot missing data, sorted by missing data
pdf("sorted_missing_data.pdf",12,6)
md[order(md$pmd),] -> smd
barplot(smd$pmd,xaxt="n",space=0,col="deepskyblue",main="Percent missing data (sorted)")
axis(1,at=(1:48)-.5,labels=smd$indiv,lwd=0,lwd.ticks=1,las=2)
dev.off()

# Plot missing data, grouped by locality
pdf("missing_data_by_locality.pdf",12,6)
barplot(md$pmd,xaxt="n",space=0,col="deepskyblue",main="Percent missing data (grouped by locality)")
axis(1,at=(1:48)-.5,labels=md$indiv,lwd=0,lwd.ticks=1,las=2)
abline(v=seq(4,44,by=4),lwd=2,col="red")
dev.off()